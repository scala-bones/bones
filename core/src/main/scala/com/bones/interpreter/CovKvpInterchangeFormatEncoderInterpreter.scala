package com.bones.interpreter

import java.time.{LocalDate, LocalDateTime}
import java.util.UUID

import com.bones.coproduct.{KeyValueDefinition, KvpCoNil, KvpCoproduct, KvpSingleValueLeft, _}
import com.bones.interpreter.CovKvpInterchangeFormatEncoderInterpreter.CovEncoder
import com.bones.interpreter.KvpInterchangeFormatValidatorInterpreter.{CoproductType, coproductTypeKey}
import shapeless.{::, Coproduct, HList, Inl, Inr, Nat}

object CovKvpInterchangeFormatEncoderInterpreter {

  trait CovEncoder[OUT, ALG[_]] {
    def encode[A](alg: ALG[A]) : A => OUT
  }
}

/**
  * Base trait for converting from HList or Case class to an interchange format such as JSON.
  * @tparam OUT The interchange format.
  */
trait CovKvpInterchangeFormatEncoderInterpreter[OUT] {

  /** This is the main entry point whose purpose is to convert
    * a schema into a function which expects the Type described by the schema (for example a case class)
    * and converts it into an interchange format library's data structure (such as Circe JSON).
    * This interpreter assumes the data has already been
    * validated and is only responsible for the data conversion.
    *
    * @param bonesSchema
    * @tparam A
    * @return
    */
  def fromSchema[A, COV[_]](bonesSchema: BonesSchema[A, COV], covEncoder: CovEncoder[OUT, COV]): A => OUT = bonesSchema match {
    case x: HListConvert[_, _, A, COV] => valueDefinition(x, covEncoder)
  }

  def none: OUT
  def empty: OUT

  /** Combine two groups of values, for instance two JSON objects into a single JSON object */
  def combine(prefix: OUT, postfix: OUT): OUT

  /** Takes a value definition and the actual value and create
    * a key value pair wrapped in the OUT type.  Analogous to
    * wrapping a key value pair in a JSON Object.
    * */
  def toObj[A, COV[_]](kvDef: KeyValueDefinition[A, COV], value: OUT, covEncoder: CovEncoder[OUT,COV]): OUT

  /** Create a function which converts a boolean into the specific OUT type */
  def booleanToOut(op: BooleanData): Boolean => OUT

  /** Create a function which converts a String into the specific OUT type */
  def stringToOut(op: StringData): String => OUT

  /** Create a function which converts an Int into the specific OUT type */
  def intToOut(op: IntData): Int => OUT

  /** Create a function which converts a Long into the specific OUT type */
  def longToOut(op: LongData): Long => OUT

  /** Create a function which converts a UUID into the specific OUT type */
  def uuidToOut(op: UuidData): UUID => OUT

  /** Create a function which converts a LocalDateTime into the specific OUT type */
  def dateTimeToOut(op: LocalDateTimeData): LocalDateTime => OUT
  def localDateToOut(op: LocalDateData): LocalDate => OUT
  def floatToOut(op: FloatData): Float => OUT
  def doubleToOut(op: DoubleData): Double => OUT
  def shortToOut(sd: ShortData): Short => OUT
  def bigDecimalToOut(op: BigDecimalData): BigDecimal => OUT
  def byteArrayToOut(ba: ByteArrayData): Array[Byte] => OUT
  def toOutList(list: List[OUT]): OUT
  def enumerationToOut[E<:Enumeration, V:Manifest](op: EnumerationData[E, V]): op.enumeration.Value => OUT
  def addStringField(element: OUT, name: String, value: String): OUT

  protected def kvpCoproduct[C <: Coproduct, COV[_]](kvpCo: KvpCoproduct[C, COV], covEncoder: CovEncoder[OUT, COV]): C => (CoproductType, OUT) = {
    kvpCo match {
      case co: KvpCoNil[COV] =>
        (input: C) => ("", empty)
      case co: KvpSingleValueLeft[l,r,COV] =>
        val fl = determineValueDefinition(co.kvpValue, covEncoder)
        val fr = kvpCoproduct(co.kvpTail, covEncoder)
        (input: C) =>
          input match {
            case Inl(l) => (co.manifestL.runtimeClass.getSimpleName, fl(l))
            case Inr(r) => fr(r)
        }
    }
  }

  /** Interpreter for the KvpHList type. */
  protected def kvpHList[H <: HList, HL <: Nat, COV[_]](
      group: KvpHList[H, HL, COV], covEncoder: CovEncoder[OUT,COV]): H => OUT =
    group match {
      case _: KvpNil[COV] =>
        (input: H) =>
          empty
      case op: KvpHListHead[out, l, h, hl, t, tl, COV] =>
        val headF = kvpHList(op.head, covEncoder)
        val tailF = kvpHList[t, tl, COV](op.tail, covEncoder)
        (input: H) =>
          {
            val l = op.split(input)
            val headOut = headF(l._1)
            val tailOut = tailF(l._2)
            combine(headOut, tailOut)
          }
      case op: KvpSingleValueHead[h, t, tl, H, COV] =>
        val valueF = determineValueDefinition(op.fieldDefinition.op, covEncoder)
        val tailF = kvpHList(op.tail, covEncoder)
        implicit val hCons = op.isHCons
        (input: H) =>
          {
            val val1 = valueF(input.head)
            val tail = tailF(input.tail)
            combine(toObj(op.fieldDefinition, val1, covEncoder), tail)
          }
      case op: KvpConcreteTypeHead[a, ht, nt, ho, xl, xll, COV] => {
        val headF = kvpHList(op.hListConvert.from, covEncoder)
        val tailF = kvpHList(op.tail, covEncoder)
        implicit val hCons = op.isHCons
        (input: a :: ht) =>
          {
            val head = headF(op.hListConvert.fAtoH(input.head))
            val tail = tailF(input.tail)
            combine(head, tail)
          }
      }
    }

  protected def determineValueDefinition[A, COV[_]](value: Either[KvpValue[A], COV[A]], covEncoder: CovEncoder[OUT, COV]): A => OUT = {
    value match {
      case Left(kvp) => valueDefinition(kvp, covEncoder)
      case Right(cov) => covEncoder.encode(cov)
    }
  }

  protected def valueDefinition[A, COV[_]](fgo: KvpValue[A], covEncoder: CovEncoder[OUT, COV]): A => OUT =
    fgo match {
      case op: OptionalKvpValueDefinition[b, COV] =>
        val valueF = determineValueDefinition(op.valueDefinitionOp, covEncoder)
        (input: A) =>
          {
            input match {
              case Some(x) => valueF(x)
              case None    => none
            }
          }
      case ob: BooleanData    => booleanToOut(ob)
      case rs: StringData     => stringToOut(rs)
      case id: IntData        => intToOut(id)
      case ri: LongData       => longToOut(ri)
      case uu: UuidData       => uuidToOut(uu)
      case dd: LocalDateTimeData   => dateTimeToOut(dd)
      case ld: LocalDateData  => localDateToOut(ld)
      case fd: FloatData      => floatToOut(fd)
      case dd: DoubleData     => doubleToOut(dd)
      case sd: ShortData      => shortToOut(sd)
      case bd: BigDecimalData => bigDecimalToOut(bd)
      case ba: ByteArrayData  => byteArrayToOut(ba)
      case ld: ListData[t, COV]    => {
        val itemToOut = determineValueDefinition(ld.tDefinition, covEncoder)
        (input: List[t]) => {
          val listOfJson = input.map(itemToOut)
          toOutList(listOfJson)
        }
      }
      case ed: EitherData[a,b,COV] =>
        val aF = determineValueDefinition[a,COV](ed.definitionA, covEncoder)
        val bF = determineValueDefinition[b,COV](ed.definitionB, covEncoder)
        (input: A) =>
          {
            input match {
              case Left(aInput)  => aF(aInput)
              case Right(bInput) => bF(bInput)
            }
          }
      case e: EnumerationData[e,a] => {
        implicit val v = e.manifestOfA
        enumerationToOut[e,a](e).asInstanceOf[A=>OUT]
      }
      case gd: KvpHListValue[h, hl, COV] =>
        val fh = kvpHList(gd.kvpHList, covEncoder)
        input => fh(input.asInstanceOf[h])
      case x: HListConvert[h, hl, A, COV] =>
        val fh = kvpHList(x.from, covEncoder)
        input: A =>
          {
            fh(x.fAtoH(input))
          }
      case c: KvpCoproductValue[c, COV] =>
        val fc = kvpCoproduct(c.kvpCoproduct, covEncoder)
        input =>
          val (name, out) = fc.apply(input.asInstanceOf[c])
          addStringField(out, coproductTypeKey, name)
      case c: KvpCoproductConvert[c,a, COV] =>
        val fc = kvpCoproduct(c.from, covEncoder)
        input: A => {
          val (name, out) = fc(c.aToC(input))
          addStringField(out, coproductTypeKey, name)
        }
    }

}
