package com.bones.interpreter

import com.bones.data.template.KvpCollectionEncoder
import com.bones.data.{KeyDefinition, _}

/**
  * Base trait for converting from HList or Case class to an interchange format such as JSON.
  *
  * @tparam OUT The interchange format.
  */
trait KvpInterchangeFormatEncoderInterpreter[ALG[_], OUT] extends KvpCollectionEncoder[ALG, OUT] {

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
    collection: PrimitiveWrapperValue[ALG, A]
  ): A => OUT = primitiveWrapperDefinition(collection)

  /** Takes a value definition and the actual value and create
    * a key value pair wrapped in the OUT type.  Analogous to
    * wrapping a key value pair in a JSON Object.
    * */
  def toObj[A](kvDef: KeyDefinition[ALG, A], value: OUT): OUT

  override def primitiveEncoder[A](keyDefinition: KeyDefinition[ALG, A]): A => OUT = {
    val f = determineValueDefinition(keyDefinition.dataDefinition)
    (a: A) =>
      {
        val value = f(a)
        toObj(keyDefinition, value)
      }
  }

  protected def determineValueDefinition[A](
    dataDefinition: Either[PrimitiveWrapperValue[ALG, A], ALG[A]]
  ): A => OUT = {
    dataDefinition match {
      case Left(kvp)  => primitiveWrapperDefinition(kvp)
      case Right(cov) => encoder.encode[A](cov)
    }
  }

  protected def primitiveWrapperDefinition[A](
    fgo: PrimitiveWrapperValue[ALG, A]
  ): A => OUT =
    fgo match {
      case op: OptionalValue[ALG, b] @unchecked =>
        val valueF = determineValueDefinition(op.valueDefinitionOp)
        (input: A) =>
          {
            input match {
              case Some(x) => valueF(x)
              case None    => interchangeFormatEncoder.none
            }
          }
      case ld: ListData[ALG, t] @unchecked => {
        val itemToOut = determineValueDefinition(ld.tDefinition)
        (input: A) =>
          {
            val listOfJson = input.asInstanceOf[List[t]].map(itemToOut)
            interchangeFormatEncoder.toOutList(listOfJson)
          }
      }
      case either: EitherData[ALG, a, b] @unchecked =>
        val aF = determineValueDefinition(either.definitionA)
        val bF = determineValueDefinition(either.definitionB)
        (input: A) =>
          {
            input match {
              case Left(aInput)  => aF(aInput)
              case Right(bInput) => bF(bInput)
            }
          }
      case gd: KvpCollectionValue[ALG, A] @unchecked =>
        val fh = fromKvpCollection[A](gd.kvpCollection)
        input =>
          fh(input)
    }

}
