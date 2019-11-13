package com.bones.sjson

import java.time.format.DateTimeFormatter
import java.util.Base64

import com.bones.data.{KvpCoNil, KvpCoproduct, KvpSingleValueLeft}
import com.bones.data._
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
}

/**
  * Encoder used to write a value directly to a String encoded JSON without a 3rd party library.
  * The string will be in compact form.
  */
trait JsonStringEncoderInterpreter {

  import JsonStringEncoderInterpreter.CoproductType

  val dateTimeFormatter: DateTimeFormatter
  val dateFormatter: DateTimeFormatter

  def fAtoString[A](bonesSchema: BonesSchema[A]): A => String =
    bonesSchema match {
      case kvp: HListConvert[h,n,a] => valueDefinition(kvp).andThen(_.getOrElse("{}"))
      case kvp: KvpCoproductConvert[c,a] => valueDefinition(kvp).andThen(_.getOrElse("{}"))
    }


  private def kvpCoproduct[C<:Coproduct](kvp: KvpCoproduct[C]): C => (CoproductType, Option[String]) = {
    kvp match {
      case KvpCoNil => _ => ("", None)
      case op: KvpSingleValueLeft[l,r] =>
        val valueF = valueDefinition(op.kvpValue)
        val valueT = kvpCoproduct(op.kvpTail)
        (input: C) =>
        {
          input match {
            case Inl(left) => (op.manifestH.runtimeClass.getSimpleName, valueF(left))
            case Inr(right) => valueT(right)
          }
        }
    }
  }
  private def kvpHList[H <: HList, HL <: Nat](group: KvpHList[H, HL]): H => Option[String] = {
    group match {
      case KvpNil                                          => _ => None
      case op: KvpSingleValueHead[h, t, tl, a]             =>
        val valueF = valueDefinition(op.fieldDefinition.op)
        val tailF = kvpHList(op.tail)
        implicit val hCons = op.isHCons
        (input: H) =>
        {
          val val1 = valueF(input.head).map("\"" + op.fieldDefinition.key + "\":"+_)
          val tail = tailF(input.tail)
          (val1, tail) match {
            case (Some(v), Some(t)) => Some(v + "," + t)
            case (None, _) => tail
            case (_,None) => val1
          }
        }
      case op: KvpHListHead[a, al, h, hl, t, tl]           =>
        val headF = kvpHList(op.head)
        val tailF = kvpHList[t, tl](op.tail)
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

      case op: KvpConcreteTypeHead[a, ht, nt, ho, xl, xll] =>
        val headF = kvpHList(op.hListConvert.from)
        val tailF = kvpHList(op.tail)
        implicit val hCons = op.isHCons
        (input: a :: ht) =>
        {
          val head = headF(op.hListConvert.fAtoH(input.head))
          val tail = tailF(input.tail)
          (head, tail) match {
            case (Some(h), Some(t)) => Some(h + t)
            case (None, _) => tail
            case (_,None) => head
          }
        }
    }
  }

  def valueDefinition[A](fgo: KvpValue[A]): A => Option[String] =
    fgo match {
      case op: OptionalKvpValueDefinition[a] =>
        val someF = valueDefinition(op.valueDefinitionOp)
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
      case ld: ListData[t]                =>
        l =>
          val tDef =  valueDefinition(ld.tDefinition)
          Some(l.flatMap(t => tDef(t)).mkString("[",",","]"))
      case dd: DoubleData                 => d => Some(d.toString)
      case fd: FloatData                  => f => Some(f.toString)
      case id: IntData                    => i => Some(i.toString)
      case sd: ShortData                  => s => Some(s.toString)
      case ed: EitherData[a, b]           =>
        val aDef: a => Option[String] = valueDefinition(ed.definitionA)
        val bDef: b => Option[String] = valueDefinition(ed.definitionB)
        _ match {
          case Left(l) => aDef(l)
          case Right(r) => bDef(r)
        }
      case ba: ByteArrayData              =>
        (input: Array[Byte]) => Some("\"" + escapeJson(Base64.getEncoder.encodeToString(input)) + "\"")
      case esd: EnumerationData[e,a]  => e => Some("\"" + escapeJson(e.toString) + "\"")
      case kvp: KvpHListValue[h, hl]      =>
        val hListDef = kvpHList(kvp.kvpHList)
        (input: A) => hListDef(input.asInstanceOf[h]).map("{" + _ + "}")
      case kvp: KvpCoproductValue[c] =>
        val coproductDef = kvpCoproduct(kvp.kvpCoproduct)
        (input: A) => {
          val (coproductType, json) = coproductDef(input.asInstanceOf[c])
          json.map(str => {
            s"""{"type":"${coproductType}", ${str.substring(1)} """
          }).orElse(Some(s"""{"type":"${coproductType}"}"""))
        }
      case x: HListConvert[a, al, b]      =>
        val fromDef = kvpHList(x.from)
        (input: A) => fromDef(x.fAtoH(input)).map("{" + _ + "}")
      case co: KvpCoproductConvert[c,a] =>
        val fromDef = kvpCoproduct(co.from)
        (input: A) => {
          val (coproductType, json) = fromDef(co.aToC(input))
          json.map(str => {
            s"""{"type":"${coproductType}", ${str.substring(1)} """
          }).orElse(Some(s"""{"type":"${coproductType}"}"""))
        }
    }





}