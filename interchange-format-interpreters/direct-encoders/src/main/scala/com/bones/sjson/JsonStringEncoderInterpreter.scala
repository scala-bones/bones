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
    override val localDateTimeFormatter: DateTimeFormatter = DateTimeFormatter.ISO_LOCAL_DATE_TIME
    override val localDateFormatter: DateTimeFormatter = DateTimeFormatter.ISO_LOCAL_DATE
    override val localTiemFormatter: DateTimeFormatter = DateTimeFormatter.ISO_LOCAL_TIME
  }

  trait CustomToJsonStringInterpreter[ALG[_]] {
    def toJsonString[A](alg: ALG[A]): A => List[String]
  }

  object NoAlgebra extends CustomToJsonStringInterpreter[NoAlgebra] {
    override def toJsonString[A](alg: NoAlgebra[A]): A => List[String] =
      sys.error("unreachable code")
  }

}

/**
  * Encoder used to write a value directly to a String encoded JSON without a 3rd party library.
  * The string will be in compact form.
  */
trait JsonStringEncoderInterpreter {

  import JsonStringEncoderInterpreter.CoproductType

  val localDateTimeFormatter: DateTimeFormatter
  val localDateFormatter: DateTimeFormatter
  val localTiemFormatter: DateTimeFormatter

  def fAtoString[ALG[_], A](
                             bonesSchema: BonesSchema[ALG, A],
                             customToJsonStringInterpreter: CustomToJsonStringInterpreter[ALG]): A => String =
    bonesSchema match {
      case kvp: HListConvert[ALG, h, n, a]@unchecked =>
        valueDefinition(kvp, customToJsonStringInterpreter).andThen(x =>
          if (x.isEmpty) "{}" else x.mkString)
      case kvp: KvpCoproductConvert[ALG, c, a]@unchecked =>
        valueDefinition(kvp, customToJsonStringInterpreter).andThen(x =>
          if (x.isEmpty) "{}" else x.mkString)
    }

  private def kvpCoproduct[ALG[_], C <: Coproduct](
                                                    kvp: KvpCoproduct[ALG, C],
                                                    customToJsonStringInterpreter: CustomToJsonStringInterpreter[ALG])
  : C => (CoproductType, List[String]) = {
    kvp match {
      case nil: KvpCoNil[_] =>
        _ =>
          ("", List.empty)
      case op: KvpSingleValueLeft[ALG, l, r]@unchecked =>
        val valueF = determineValueDefinition[ALG, l](op.kvpValue, customToJsonStringInterpreter)
        val valueT = kvpCoproduct(op.kvpTail, customToJsonStringInterpreter)
        (input: C) => {
          input match {
            case Inl(left) => (op.manifestL.runtimeClass.getSimpleName, valueF(left))
            case Inr(right) => valueT(right)
          }
        }
    }
  }

  private def kvpHList[ALG[_], H <: HList, HL <: Nat](
                                                       group: KvpHList[ALG, H, HL],
                                                       customInterpreter: CustomToJsonStringInterpreter[ALG]
                                                     ): H => List[String] = {

    group match {
      case nil: KvpNil[_] =>
        _ =>
          List.empty
      case op: KvpSingleValueHead[ALG, h, t, tl, a]@unchecked =>
        val valueF = determineValueDefinition(op.fieldDefinition.op, customInterpreter)
        val tailF = kvpHList(op.tail, customInterpreter)
        implicit val hCons = op.isHCons
        (inputH: H) => {
          val input = inputH.asInstanceOf[a]
          val val1 = valueF(input.head)
          val valWithKey =
            if (val1.isEmpty) val1
            else  List("\"", op.fieldDefinition.key, "\":") ::: val1
          val tail = tailF(input.tail)
          if (!valWithKey.isEmpty && !tail.isEmpty)
            valWithKey ::: "," :: tail
          else
            valWithKey ::: tail

        }
      case op: KvpHListHead[ALG, a, al, h, hl, t, tl]@unchecked =>
        val headF = kvpHList(op.head, customInterpreter)
        val tailF = kvpHList[ALG, t, tl](op.tail, customInterpreter)
        (input: H) => {
          val l = op.split(input)
          val headOut = headF(l._1)
          val tailOut = tailF(l._2)
          if (!headOut.isEmpty && !tailOut.isEmpty)
            headOut ::: "," :: tailOut
          else
            headOut ::: tailOut
        }

      case op: KvpConcreteTypeHead[ALG, a, ht, nt]@unchecked =>
        val headF: a => List[String] = fromBonesSchema(op.bonesSchema, customInterpreter)
        val tailF = kvpHList(op.tail, customInterpreter)
        implicit val hCons = op.isHCons
        (input: a :: ht) => {
          val head = headF(input.head)
          val tail = tailF(input.tail)
          head ::: tail
        }
    }
  }

  def fromBonesSchema[ALG[_], A](
                                  bonesSchema: BonesSchema[ALG, A],
                                  customToJsonStringInterpreter: CustomToJsonStringInterpreter[ALG]
                                ): A => List[String] = {
    bonesSchema match {
      case kvp: KvpCoproductConvert[ALG, c, a] =>
        val f = kvpCoproduct(kvp.from, customToJsonStringInterpreter)
        (input: A) => {
          val (coproductType, json) = f(kvp.aToC(input))
          List(s""" "type": "${coproductType}", ${json} """)
        }
      case hListConvert: HListConvert[ALG, h, n, a] =>
        val f = kvpHList(hListConvert.from, customToJsonStringInterpreter)
        (input: A) => {
          f(hListConvert.fAtoH(input))
        }
    }
  }

  def determineValueDefinition[ALG[_], A](
                                           kvpValue: Either[KvpValue[A], ALG[A]],
                                           customInterpreter: CustomToJsonStringInterpreter[ALG]
                                         ): A => List[String] = {
    kvpValue match {
      case Left(kvp) => valueDefinition(kvp, customInterpreter)
      case Right(alg) => customInterpreter.toJsonString(alg)
    }
  }

  def valueDefinition[ALG[_], A](
                                  fgo: KvpValue[A],
                                  customToJsonStringInterpreter: CustomToJsonStringInterpreter[ALG]): A => List[String] =
    fgo match {
      case op: OptionalKvpValueDefinition[ALG, a]@unchecked =>
        val someF = determineValueDefinition(op.valueDefinitionOp, customToJsonStringInterpreter)
        _ match {
          case Some(s) => someF(s)
          case None => List.empty
        }
      case ob: BooleanData =>
        b =>
          if (b) List("true") else List("false")
      case rs: StringData =>
        s =>
          List("\"" + escapeJson(s) + "\"")
      case ri: LongData =>
        l =>
          List(l.toString)
      case uu: UuidData =>
        u =>
          List("\"" + u.toString + "\"")
      case ld: LocalDateData =>
        d =>
          List("\"" + escapeJson(localDateFormatter.format(d)) + "\"")
      case dd: LocalDateTimeData =>
        d =>
          List("\"" + escapeJson(localDateTimeFormatter.format(d)) + "\"")
      case lt: LocalTimeData =>
        d =>
          List("\"" + escapeJson(localTiemFormatter.format(d)) + "\"")
      case bd: BigDecimalData =>
        bd =>
          List(bd.toString)
      case ld: ListData[ALG, t]@unchecked =>
        l =>
          val tDef = determineValueDefinition(ld.tDefinition, customToJsonStringInterpreter)
          List(l.flatMap(t => tDef(t)).mkString("[", ",", "]"))
      case dd: DoubleData =>
        d =>
          List(d.toString)
      case fd: FloatData =>
        f =>
          List(f.toString)
      case id: IntData =>
        i =>
          List(i.toString)
      case sd: ShortData =>
        s =>
          List(s.toString)
      case ed: EitherData[ALG, a, b]@unchecked =>
        val aDef: a => List[String] =
          determineValueDefinition(ed.definitionA, customToJsonStringInterpreter)
        val bDef: b => List[String] =
          determineValueDefinition(ed.definitionB, customToJsonStringInterpreter)
        _ match {
          case Left(l) => aDef(l)
          case Right(r) => bDef(r)
        }
      case ba: ByteArrayData =>
        (input: Array[Byte]) =>
          List("\"", escapeJson(Base64.getEncoder.encodeToString(input)), "\"")
      case esd: EnumerationData[e, a] =>
        e =>
          List("\"", escapeJson(e.toString), "\"")
      case kvp: KvpHListValue[ALG, h, hl]@unchecked =>
        val hListDef = kvpHList(kvp.kvpHList, customToJsonStringInterpreter)
        (input: A) =>
          hListDef(input.asInstanceOf[h]).flatMap(x => List("{", x, "}"))
      case kvp: KvpCoproductValue[ALG, c]@unchecked =>
        val coproductDef = kvpCoproduct(kvp.kvpCoproduct, customToJsonStringInterpreter)
        (input: A) => {
          val (coproductType, json) = coproductDef(input.asInstanceOf[c])
          if (json.isEmpty)
            List(s"""{"type":"${coproductType}"}""")
          else {
            val dropOpeningBracket = json.headOption.map(_.substring(1)).toList ::: json.tail
            List("""{"type":"""", coproductType, """", """) ::: dropOpeningBracket ::: " " :: Nil
          }
        }
      case x: HListConvert[ALG, a, al, b]@unchecked =>
        val fromDef = kvpHList(x.from, customToJsonStringInterpreter)
        (input: A) => {
          val h = x.fAtoH(input)
          "{" :: fromDef(h) ::: "}" :: Nil
        }
      case co: KvpCoproductConvert[ALG, c, a]@unchecked =>
        val fromDef = kvpCoproduct(co.from, customToJsonStringInterpreter)
        (input: A) => {
          val (coproductType, json) = fromDef(co.aToC(input))
          if (json.isEmpty)
            List(s"""{"type":"${coproductType}"}""")
          else {
            val dropOpeningBracket = json.headOption.map(_.substring(1)).toList ::: json.tail
            List("""{"type":"""", coproductType, """", """) ::: dropOpeningBracket ::: " " :: Nil
          }

        }
    }

}
