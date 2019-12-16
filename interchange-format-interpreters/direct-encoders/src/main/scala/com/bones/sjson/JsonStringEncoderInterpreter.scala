package com.bones.sjson

import java.time.format.DateTimeFormatter
import java.util.Base64

import com.bones.data.{KvpCoNil, KvpCoproduct, KvpSingleValueLeft}
import com.bones.data._
import com.bones.sjson.JsonStringEncoderInterpreter.CustomToJsonStringInterpreter
import com.bones.syntax.NoAlgebra
import shapeless.{::, Coproduct, HList, Inl, Inr, Nat}
import org.apache.commons.text.StringEscapeUtils.escapeJson

object JsonStringEncoderInterpreter {

  type CoproductType = String

  /**
    * Implementation of the JsonStringEncoderInterpreter assuming dates are String with Iso format.
    */
  val isoEncoder = new JsonStringEncoderInterpreter {
    override val dateTimeFormatter: DateTimeFormatter = DateTimeFormatter.ISO_DATE_TIME
    override val dateFormatter: DateTimeFormatter = DateTimeFormatter.ISO_DATE
  }

  trait CustomToJsonStringInterpreter[ALG[_]] {
    def toJsonString[A](alg: ALG[A]): A => Option[String]
  }

  object NoAlgebra extends CustomToJsonStringInterpreter[NoAlgebra] {
    override def toJsonString[A](alg: NoAlgebra[A]): A => Option[String] = sys.error("unreachable code")
  }
}

/**
  * Encoder used to write a value directly to a String encoded JSON without a 3rd party library.
  * The string will be in compact form.
  */
trait JsonStringEncoderInterpreter {

  import JsonStringEncoderInterpreter.CoproductType

  val dateTimeFormatter: DateTimeFormatter
  val dateFormatter: DateTimeFormatter

  def fAtoString[ALG[_], A](bonesSchema: BonesSchema[ALG,A], customToJsonStringInterpreter: CustomToJsonStringInterpreter[ALG]): A => String =
    bonesSchema match {
      case kvp: HListConvert[ALG, h,n,a] @unchecked => valueDefinition(kvp, customToJsonStringInterpreter).andThen(_.getOrElse("{}"))
      case kvp: KvpCoproductConvert[ALG,c,a] @unchecked => valueDefinition(kvp, customToJsonStringInterpreter).andThen(_.getOrElse("{}"))
    }


  private def kvpCoproduct[ALG[_],C<:Coproduct](kvp: KvpCoproduct[ALG,C], customToJsonStringInterpreter: CustomToJsonStringInterpreter[ALG]): C => (CoproductType, Option[String]) = {
    kvp match {
      case nil: KvpCoNil[_] => _ => ("", None)
      case op: KvpSingleValueLeft[ALG, l,r] @unchecked =>
        val valueF = determineValueDefinition[ALG,l](op.kvpValue, customToJsonStringInterpreter)
        val valueT = kvpCoproduct(op.kvpTail, customToJsonStringInterpreter)
        (input: C) =>
        {
          input match {
            case Inl(left) => (op.manifestL.runtimeClass.getSimpleName, valueF(left))
            case Inr(right) => valueT(right)
          }
        }
    }
  }
  private def kvpHList[ALG[_], H <: HList, HL <: Nat]
    (
      group: KvpHList[ALG, H, HL],
      customInterpreter: CustomToJsonStringInterpreter[ALG]
    ): H => Option[String] = {

    group match {
      case nil: KvpNil[_]  => _ => None
      case op: KvpSingleValueHead[ALG, h, t, tl, a] @unchecked =>
        val valueF = determineValueDefinition(op.fieldDefinition.op, customInterpreter)
        val tailF = kvpHList(op.tail, customInterpreter)
        implicit val hCons = op.isHCons
        (inputH: H) =>
        {
          val input = inputH.asInstanceOf[a]
          val val1 = valueF(input.head).map("\"" + op.fieldDefinition.key + "\":"+_)
          val tail = tailF(input.tail)
          (val1, tail) match {
            case (Some(v), Some(t)) => Some(v + "," + t)
            case (None, _) => tail
            case (_,None) => val1
          }
        }
      case op: KvpHListHead[ALG, a, al, h, hl, t, tl] @unchecked =>
        val headF = kvpHList(op.head, customInterpreter)
        val tailF = kvpHList[ALG, t, tl](op.tail, customInterpreter)
        (input: H) =>
        {
          val l = op.split(input)
          val headOut = headF(l._1)
          val tailOut = tailF(l._2)
          (headOut, tailOut) match {
            case (Some(h), Some(t)) => Some(h + "," + t)
            case (None, _) => tailOut
            case (_,None) => headOut
          }
        }

      case op: KvpConcreteTypeHead[ALG, a, ht, nt] @unchecked =>
        val headF: a => Option[String] = fromBonesSchema(op.bonesSchema, customInterpreter)
        val tailF = kvpHList(op.tail, customInterpreter)
        implicit val hCons = op.isHCons
        (input: a :: ht) =>
        {
          val head = headF(input.head)
          val tail = tailF(input.tail)
          (head, tail) match {
            case (Some(h), Some(t)) => Some(h + t)
            case (None, _) => tail
            case (_,None) => head
          }
        }
    }
  }

  def fromBonesSchema[ALG[_], A]
    (
      bonesSchema: BonesSchema[ALG, A],
      customToJsonStringInterpreter: CustomToJsonStringInterpreter[ALG]
    ): A => Option[String] = {
      bonesSchema match {
        case kvp: KvpCoproductConvert[ALG, c, a] =>
          val f = kvpCoproduct(kvp.from, customToJsonStringInterpreter)
          (input: A) => {
            val (coproductType, json) = f(kvp.aToC(input))
            Some(s""" "type": "${coproductType}", ${json} """)
          }
        case hListConvert: HListConvert[ALG, h, n, a] =>
          val f = kvpHList(hListConvert.from, customToJsonStringInterpreter)
          (input: A) => {
            f(hListConvert.fAtoH(input))
          }
      }
    }

  def determineValueDefinition[ALG[_], A]
    (
      kvpValue: Either[KvpValue[A], ALG[A]],
      customInterpreter: CustomToJsonStringInterpreter[ALG]
    ): A => Option[String] = {
    kvpValue match {
      case Left(kvp) => valueDefinition(kvp, customInterpreter)
      case Right(alg) => customInterpreter.toJsonString(alg)
    }
  }

  def valueDefinition[ALG[_],A](fgo: KvpValue[A], customToJsonStringInterpreter: CustomToJsonStringInterpreter[ALG]): A => Option[String] =
    fgo match {
      case op: OptionalKvpValueDefinition[ALG,a] @unchecked =>
        val someF = determineValueDefinition(op.valueDefinitionOp, customToJsonStringInterpreter)
        _ match {
          case Some(s) => someF(s)
          case None => None
        }
      case ob: BooleanData                => b => if (b) Some("true") else Some("false")
      case rs: StringData                 => s => Some("\"" + escapeJson(s) + "\"")
      case ri: LongData                   => l => Some(l.toString)
      case uu: UuidData                   => u => Some("\"" + u.toString + "\"")
      case ld: LocalDateData              => d => Some("\"" + escapeJson(dateFormatter.format(d)) + "\"")
      case dd: LocalDateTimeData          =>  d => Some("\"" + escapeJson(dateTimeFormatter.format(d)) + "\"")
      case bd: BigDecimalData             => bd => Some(bd.toString)
      case ld: ListData[ALG, t] @unchecked =>
        l =>
          val tDef =  determineValueDefinition(ld.tDefinition, customToJsonStringInterpreter)
          Some(l.flatMap(t => tDef(t)).mkString("[",",","]"))
      case dd: DoubleData                 => d => Some(d.toString)
      case fd: FloatData                  => f => Some(f.toString)
      case id: IntData                    => i => Some(i.toString)
      case sd: ShortData                  => s => Some(s.toString)
      case ed: EitherData[ALG, a, b] @unchecked =>
        val aDef: a => Option[String] = determineValueDefinition(ed.definitionA, customToJsonStringInterpreter)
        val bDef: b => Option[String] = determineValueDefinition(ed.definitionB, customToJsonStringInterpreter)
        _ match {
          case Left(l) => aDef(l)
          case Right(r) => bDef(r)
        }
      case ba: ByteArrayData              =>
        (input: Array[Byte]) => Some("\"" + escapeJson(Base64.getEncoder.encodeToString(input)) + "\"")
      case esd: EnumerationData[e,a]  => e => Some("\"" + escapeJson(e.toString) + "\"")
      case kvp: KvpHListValue[ALG, h, hl] @unchecked =>
        val hListDef = kvpHList(kvp.kvpHList, customToJsonStringInterpreter)
        (input: A) => hListDef(input.asInstanceOf[h]).map("{" + _ + "}")
      case kvp: KvpCoproductValue[ALG, c] @unchecked =>
        val coproductDef = kvpCoproduct(kvp.kvpCoproduct, customToJsonStringInterpreter)
        (input: A) => {
          val (coproductType, json) = coproductDef(input.asInstanceOf[c])
          json.map(str => {
            s"""{"type":"${coproductType}", ${str.substring(1)} """
          }).orElse(Some(s"""{"type":"${coproductType}"}"""))
        }
      case x: HListConvert[ALG, a, al, b] @unchecked =>
        val fromDef = kvpHList(x.from, customToJsonStringInterpreter)
        (input: A) => fromDef(x.fAtoH(input)).map("{" + _ + "}")
      case co: KvpCoproductConvert[ALG, c,a] @unchecked =>
        val fromDef = kvpCoproduct(co.from, customToJsonStringInterpreter)
        (input: A) => {
          val (coproductType, json) = fromDef(co.aToC(input))
          json.map(str => {
            s"""{"type":"${coproductType}", ${str.substring(1)} """
          }).orElse(Some(s"""{"type":"${coproductType}"}"""))
        }
    }





}