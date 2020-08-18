package com.bones.sjson

import java.time.format.DateTimeFormatter
import java.util.Base64

import com.bones.data.{KvpCoNil, KvpCoproduct, KvpCoproductCollectionHead}
import com.bones.data._
import com.bones.data.values.CNilF
import com.bones.sjson.JsonStringEncoderInterpreter.CustomToJsonStringInterpreter
import shapeless.{:+:, ::, Coproduct, HList, Inl, Inr, Nat}
import org.apache.commons.text.StringEscapeUtils.escapeJson

object JsonStringEncoderInterpreter {

  type CoproductType = String

  /**
    * Implementation of the JsonStringEncoderInterpreter assuming dates are String with Iso format.
    */
  val isoEncoder = new JsonStringEncoderInterpreter {
    override val localDateTimeFormatter: DateTimeFormatter = DateTimeFormatter.ISO_LOCAL_DATE_TIME
    override val localDateFormatter: DateTimeFormatter = DateTimeFormatter.ISO_LOCAL_DATE
    override val localTimeFormatter: DateTimeFormatter = DateTimeFormatter.ISO_LOCAL_TIME
  }

  object CustomToJsonStringInterpreter {

    /** using kind projector allows us to create a new interpreter by merging two existing interpreters.
      * see https://stackoverflow.com/a/60561575/387094
      * */
    def merge[L[_], R[_] <: Coproduct, A](
      li: CustomToJsonStringInterpreter[L],
      ri: CustomToJsonStringInterpreter[R])
      : CustomToJsonStringInterpreter[Lambda[A => L[A] :+: R[A]]] =
      new CustomToJsonStringInterpreter[Lambda[A => L[A] :+: R[A]]] {
        override def toJsonString[A](lr: L[A] :+: R[A]): A => List[String] = lr match {
          case Inl(l) => li.toJsonString(l)
          case Inr(r) => ri.toJsonString(r)
        }
      }

    implicit class InterpreterOps[ALG[_]](val base: CustomToJsonStringInterpreter[ALG])
        extends AnyVal {
      def ++[R[_] <: Coproduct](r: CustomToJsonStringInterpreter[R])
        : CustomToJsonStringInterpreter[Lambda[A => ALG[A] :+: R[A]]] =
        merge(base, r)
    }

    object CNilCustomEncoder extends CustomToJsonStringInterpreter[CNilF] {
      override def toJsonString[A](alg: CNilF[A]): A => List[String] = sys.error("Unreachable code")
    }
  }

  trait CustomToJsonStringInterpreter[ALG[_]] {
    def toJsonString[A](alg: ALG[A]): A => List[String]
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
  val localTimeFormatter: DateTimeFormatter

  def fAtoString[ALG[_], A](
    bonesSchema: PrimitiveWrapperValue[ALG, A],
    customToJsonStringInterpreter: CustomToJsonStringInterpreter[ALG]): A => String =
    bonesSchema match {
      case kvp: SwitchEncoding[ALG, h, n, a] @unchecked =>
        valueDefinition(kvp, customToJsonStringInterpreter).andThen(x =>
          if (x.isEmpty) "{}" else x.mkString)
      case kvp: CoproductSwitch[ALG, c, a] @unchecked =>
        valueDefinition(kvp, customToJsonStringInterpreter).andThen(x =>
          if (x.isEmpty) "{}" else x.mkString)
      case _ => ??? // TODO
    }

  private def kvpCoproduct[ALG[_], C <: Coproduct](
    kvp: KvpCoproduct[ALG, C],
    customToJsonStringInterpreter: CustomToJsonStringInterpreter[ALG])
    : C => (CoproductType, List[String]) = {
    kvp match {
      case nil: KvpCoNil[_] =>
        _ =>
          ("", List.empty)
      case op: KvpCoproductCollectionHead[ALG, l, r] @unchecked =>
        val valueF = determineValueDefinition[ALG, l](op.kvpValue, customToJsonStringInterpreter)
        val valueT = kvpCoproduct(op.kvpTail, customToJsonStringInterpreter)
        (input: C) =>
          {
            input match {
              case Inl(left)  => (op.manifestL.runtimeClass.getSimpleName, valueF(left))
              case Inr(right) => valueT(right)
            }
          }
    }
  }

  private def kvpHList[ALG[_], H <: HList, HL <: Nat](
    group: KvpHListCollection[ALG, H, HL],
    customInterpreter: CustomToJsonStringInterpreter[ALG]
  ): H => List[String] = {

    group match {
      case nil: KvpNil[_] =>
        _ =>
          List.empty
      case op: KvpSingleValueHead[ALG, h, t, tl, a] @unchecked =>
        val valueF = determineValueDefinition(op.fieldDefinition.dataDefinition, customInterpreter)
        val tailF = kvpHList(op.tail, customInterpreter)
        implicit val hCons = op.isHCons
        (inputH: H) =>
          {
            val input = inputH.asInstanceOf[a]
            val val1 = valueF(input.head)
            val valWithKey =
              if (val1.isEmpty) val1
              else List("\"", op.fieldDefinition.key, "\":") ::: val1
            val tail = tailF(input.tail)
            if (!valWithKey.isEmpty && !tail.isEmpty)
              valWithKey ::: "," :: tail
            else
              valWithKey ::: tail

          }
      case op: KvpHListCollectionHead[ALG, a, al, h, hl, t, tl] @unchecked =>
        val headF = kvpHList(op.head, customInterpreter)
        val tailF = kvpHList[ALG, t, tl](op.tail, customInterpreter)
        (input: H) =>
          {
            val l = op.split(input)
            val headOut = headF(l._1)
            val tailOut = tailF(l._2)
            if (!headOut.isEmpty && !tailOut.isEmpty)
              headOut ::: "," :: tailOut
            else
              headOut ::: tailOut
          }

      case op: KvpConcreteValueHead[ALG, a, ht, nt] @unchecked =>
        val headF: a => List[String] = fromBonesSchema(op.collection, customInterpreter)
        val tailF = kvpHList(op.wrappedEncoding, customInterpreter)
        implicit val hCons = op.isHCons
        (input: a :: ht) =>
          {
            val head = headF(input.head)
            val tail = tailF(input.tail)
            head ::: tail
          }
    }
  }

  def fromBonesSchema[ALG[_], A](
    bonesSchema: PrimitiveWrapperValue[ALG, A],
    customToJsonStringInterpreter: CustomToJsonStringInterpreter[ALG]
  ): A => List[String] = {
    bonesSchema match {
      case kvp: CoproductSwitch[ALG, c, a] =>
        val f = kvpCoproduct(kvp.from, customToJsonStringInterpreter)
        (input: A) =>
          {
            val (coproductType, json) = f(kvp.aToC(input))
            List(s""" "type": "${coproductType}", ${json} """)
          }
      case hListConvert: SwitchEncoding[ALG, h, n, a] =>
        val f = kvpHList(hListConvert.from, customToJsonStringInterpreter)
        (input: A) =>
          {
            f(hListConvert.fAtoH(input))
          }
      case _ => ??? //TODO
    }
  }

  def determineValueDefinition[ALG[_], A](
    kvpValue: Either[PrimitiveWrapperValue[ALG, A], ALG[A]],
    customInterpreter: CustomToJsonStringInterpreter[ALG]
  ): A => List[String] = {
    kvpValue match {
      case Left(kvp)  => valueDefinition(kvp, customInterpreter)
      case Right(alg) => customInterpreter.toJsonString(alg)
    }
  }

  def valueDefinition[ALG[_], A](
    fgo: PrimitiveWrapperValue[ALG, A],
    customToJsonStringInterpreter: CustomToJsonStringInterpreter[ALG]): A => List[String] =
    fgo match {
      case op: OptionalValue[ALG, a] @unchecked =>
        val someF = determineValueDefinition(op.valueDefinitionOp, customToJsonStringInterpreter)
        _ match {
          case Some(s) => someF(s)
          case None    => List.empty
        }
      case ld: ListData[ALG, t] @unchecked =>
        l =>
          val tDef = determineValueDefinition(ld.tDefinition, customToJsonStringInterpreter)
          List(l.flatMap(t => tDef(t)).mkString("[", ",", "]"))
      case ed: EitherData[ALG, a, b] @unchecked =>
        val aDef: a => List[String] =
          determineValueDefinition(ed.definitionA, customToJsonStringInterpreter)
        val bDef: b => List[String] =
          determineValueDefinition(ed.definitionB, customToJsonStringInterpreter)
        _ match {
          case Left(l)  => aDef(l)
          case Right(r) => bDef(r)
        }
      case kvp: KvpCollectionValue[ALG, h, hl] @unchecked =>
        val hListDef = kvpHList(kvp.kvpCollection, customToJsonStringInterpreter)
        (input: A) =>
          hListDef(input.asInstanceOf[h]).flatMap(x => List("{", x, "}"))
      case kvp: CoproductCollection[ALG, c] @unchecked =>
        val coproductDef = kvpCoproduct(kvp.kvpCoproduct, customToJsonStringInterpreter)
        (input: A) =>
          {
            val (coproductType, json) = coproductDef(input.asInstanceOf[c])
            if (json.isEmpty)
              List(s"""{"type":"${coproductType}"}""")
            else {
              val dropOpeningBracket = json.headOption.map(_.substring(1)).toList ::: json.tail
              List("""{"type":"""", coproductType, """", """) ::: dropOpeningBracket ::: " " :: Nil
            }
          }
      case x: SwitchEncoding[ALG, a, al, b] @unchecked =>
        val fromDef = kvpHList(x.from, customToJsonStringInterpreter)
        (input: A) =>
          {
            val h = x.fAtoH(input)
            "{" :: fromDef(h) ::: "}" :: Nil
          }
      case co: CoproductSwitch[ALG, c, a] @unchecked =>
        val fromDef = kvpCoproduct(co.from, customToJsonStringInterpreter)
        (input: A) =>
          {
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
