package com.bones.interpreter.encoder

import com.bones.data.template.KvpCollectionEncoder
import com.bones.data._

/** Base trait for converting from HList or Case class to an interchange format such as JSON.
  *
  * @tparam OUT The interchange format.
  */
trait KvpInterchangeFormatEncoderInterpreter[ALG[_], OUT]
    extends KvpCollectionEncoder[String, ALG, OUT] {

  def coproductTypeKey: String

  def encoder: InterchangeFormatEncoderValue[ALG, OUT]

  def interchangeFormatEncoder: InterchangeFormatPrimitiveEncoder[OUT]

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
  def generateEncoder[A](
    collection: KvpCollection[String, ALG, A]
  ): Encoder[ALG, A, OUT] = fromKvpCollection(collection)

  /** Takes a value definition and the actual value and create
    * a key value pair wrapped in the OUT type.  Analogous to
    * wrapping a key value pair in a JSON Object.
    */
  def toObj[A](kvDef: KeyDefinition[String, ALG, A], value: OUT): OUT

  override def primitiveEncoder[A](
    keyDefinition: KeyDefinition[String, ALG, A]
  ): Encoder[ALG, A, OUT] = {
    val f = determineValueDefinition(keyDefinition.dataDefinition)
    (a: A) =>
      {
        val value = f.encode(a)
        toObj(keyDefinition, value)
      }
  }

  protected def determineValueDefinition[A](
    dataDefinition: Either[HigherOrderValue[String, ALG, A], ALG[A]]
  ): Encoder[ALG, A, OUT] = {
    dataDefinition match {
      case Left(kvp)  => primitiveWrapperDefinition(kvp)
      case Right(cov) => encoder.generateEncoder[A](cov)
    }
  }

  protected def primitiveWrapperDefinition[A](
    fgo: HigherOrderValue[String, ALG, A]
  ): Encoder[ALG, A, OUT] =
    fgo match {
      case op: OptionalValue[String, ALG, b] @unchecked =>
        val encoder = determineValueDefinition(op.valueDefinitionOp)
        (input: A) =>
          {
            input match {
              case Some(x) => encoder.encode(x)
              case None    => interchangeFormatEncoder.none
            }
          }
      case ld: ListData[String, ALG, t] @unchecked =>
        val itemToOut = determineValueDefinition(ld.tDefinition)
        (input: A) =>
          {
            val listOfJson = input.asInstanceOf[List[t]].map(itemToOut.encode)
            interchangeFormatEncoder.toOutList(listOfJson)
          }
      case either: EitherData[String, ALG, a, b] @unchecked =>
        val aF = determineValueDefinition(either.definitionA)
        val bF = determineValueDefinition(either.definitionB)
        (input: A) =>
          {
            input match {
              case Left(aInput)  => aF.encode(aInput)
              case Right(bInput) => bF.encode(bInput)
            }
          }
      case gd: KvpCollectionValue[String, ALG, A] @unchecked =>
        val fh = fromKvpCollection[A](gd.kvpCollection)
        input =>
          fh.encode(input)
    }

}
