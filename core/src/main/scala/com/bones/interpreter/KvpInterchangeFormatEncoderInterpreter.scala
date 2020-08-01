package com.bones.interpreter

import com.bones.data.values.CNilF
import com.bones.data.{KeyValueDefinition, KvpCoNil, KvpCoproduct, KvpSingleValueLeft, _}
import com.bones.interpreter.KvpInterchangeFormatValidatorInterpreter.CoproductType
import shapeless.{:+:, Coproduct, HList, Inl, Inr, Nat}

/**
  * Base trait for converting from HList or Case class to an interchange format such as JSON.
  *
  * @tparam OUT The interchange format.
  */
trait KvpInterchangeFormatEncoderInterpreter[OUT] {

  val coproductTypeKey: String

  /** This is the main entry point whose purpose is to convert
    * a schema into a function which expects the Type described by the schema (for example a case class)
    * and converts it into an interchange format library's data structure (such as Circe JSON).
    * This interpreter assumes the data has already been
    * validated and is only responsible for the data conversion.
    *
    * @param collection
    * @tparam A
    * @return
    */
  def generateEncoder[ALG[_], A](
    collection: KvpCollection[ALG, A],
    covEncoder: InterchangeFormatEncoderValue[ALG, OUT]
  ): A => OUT = valueDefinition(collection, covEncoder)

  def none: OUT

  def empty: OUT

  /** Combine two groups of values, for instance two JSON objects into a single JSON object */
  def combine(prefix: OUT, postfix: OUT): OUT

  /** Takes a value definition and the actual value and create
    * a key value pair wrapped in the OUT type.  Analogous to
    * wrapping a key value pair in a JSON Object.
    * */
  def toObj[ALG[_], A](kvDef: KeyValueDefinition[ALG, A], value: OUT): OUT

  /** Create a function which converts a boolean into the specific OUT type */
  def booleanToOut: Boolean => OUT

  /** Create a function which converts a String into the specific OUT type */
  def stringToOut: String => OUT

  /** Create a function which converts an Int into the specific OUT type */
  def intToOut: Int => OUT

  /** Create a function which converts a Long into the specific OUT type */
  def longToOut: Long => OUT

  def floatToOut: Float => OUT

  def doubleToOut: Double => OUT

  def shortToOut: Short => OUT

  def bigDecimalToOut: BigDecimal => OUT

  def byteArrayToOut: Array[Byte] => OUT

  def toOutList(list: List[OUT]): OUT

  def addStringField(element: OUT, name: String, value: String): OUT

  def kvpCoproduct[ALG[_], C <: Coproduct](
    kvpCo: KvpCoproduct[ALG, C],
    encoder: InterchangeFormatEncoderValue[ALG, OUT]
  ): C => (CoproductType, OUT) = {
    kvpCo match {
      case co: KvpCoNil[_] =>
        (input: C) =>
          ("", empty)
      case co: KvpSingleValueLeft[ALG, l, r] =>
        val fl: l => OUT = determineValueDefinition(co.kvpValue, encoder)
        val fr: r => (CoproductType, OUT) = kvpCoproduct(co.kvpTail, encoder)
        (input: C) =>
          input match {
            case Inl(l) => (co.manifestL.runtimeClass.getSimpleName, fl(l))
            case Inr(r) => fr(r)
          }
    }
  }

  /** Interpreter for the KvpHList type. */
  protected def kvpHList[ALG[_], H <: HList, HL <: Nat](
    group: KvpHList[ALG, H, HL],
    encoder: InterchangeFormatEncoderValue[ALG, OUT]
  ): H => OUT =
    group match {
      case nil: KvpNil[_] =>
        (input: H) =>
          empty
      case op: KvpHListHead[ALG, out, l, h, hl, t, tl] =>
        val headF = kvpHList(op.head, encoder)
        val tailF = kvpHList[ALG, t, tl](op.tail, encoder)
        (input: H) =>
          {
            val l = op.split(input)
            val headOut = headF(l._1)
            val tailOut = tailF(l._2)
            combine(headOut, tailOut)
          }
      case op: KvpSingleValueHead[ALG, h, t, tl, H] =>
        val valueF = determineValueDefinition(op.fieldDefinition.dataDefinition, encoder)
        val tailF = kvpHList(op.tail, encoder)
        implicit val hCons = op.isHCons
        (input: H) =>
          {
            val val1 = valueF(hCons.head(input))
            val tailCons = hCons.tail(input)
            val tail = tailF(tailCons)
            combine(toObj(op.fieldDefinition, val1), tail)
          }
      case op: KvpCollectionHead[ALG, H, ht, nt] @unchecked => {
        val headF = generateEncoder(op.collection, encoder)
        val tailF = kvpHList(op.tail, encoder)
        implicit val hCons = op.isHCons
        (input: H) =>
          {
            val head = headF(hCons.head(input))
            val tail = tailF(hCons.tail(input))
            combine(head, tail)
          }
      }
    }

  protected def determineValueDefinition[ALG[_], A](
    dataDefinition: Either[KvpCollection[ALG, A], ALG[A]],
    algEncoder: InterchangeFormatEncoderValue[ALG, OUT]
  ): A => OUT = {
    dataDefinition match {
      case Left(kvp)  => valueDefinition(kvp, algEncoder)
      case Right(cov) => algEncoder.encode[A](cov)
    }
  }

  protected def valueDefinition[ALG[_], A](
    fgo: KvpCollection[ALG, A],
    encoder: InterchangeFormatEncoderValue[ALG, OUT]
  ): A => OUT =
    fgo match {
      case op: OptionalKvpValueDefinition[ALG, b] @unchecked =>
        val valueF = determineValueDefinition(op.valueDefinitionOp, encoder)
        (input: A) =>
          {
            input match {
              case Some(x) => valueF(x)
              case None    => none
            }
          }
      case ld: ListData[ALG, t] @unchecked => {
        val itemToOut = determineValueDefinition(ld.tDefinition, encoder)
        (input: List[t]) =>
          {
            val listOfJson = input.map(itemToOut)
            toOutList(listOfJson)
          }
      }
      case either: EitherData[ALG, a, b] @unchecked =>
        val aF = determineValueDefinition(either.definitionA, encoder)
        val bF = determineValueDefinition(either.definitionB, encoder)
        (input: A) =>
          {
            input match {
              case Left(aInput)  => aF(aInput)
              case Right(bInput) => bF(bInput)
            }
          }
      case gd: KvpHListValue[ALG, h, hl] @unchecked =>
        val fh = kvpHList(gd.kvpHList, encoder)
        input =>
          fh(input.asInstanceOf[h])
      case x: HListConvert[ALG, h, hl, A] @unchecked =>
        val fh = kvpHList(x.from, encoder)
        input: A =>
          {
            fh(x.fAtoH(input))
          }
      case c: KvpCoproductValue[ALG, c] @unchecked =>
        val fc = kvpCoproduct(c.kvpCoproduct, encoder)
        input =>
          val (name, out) = fc.apply(input.asInstanceOf[c])
          addStringField(out, coproductTypeKey, name)
      case c: KvpCoproductConvert[ALG, c, a] @unchecked =>
        val fc = kvpCoproduct(c.from, encoder)
        input: A =>
          {
            val (name, out) = fc(c.aToC(input))
            addStringField(out, coproductTypeKey, name)
          }
    }

}
