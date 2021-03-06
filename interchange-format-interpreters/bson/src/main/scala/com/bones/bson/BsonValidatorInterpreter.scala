package com.bones.bson

import com.bones.data.Error._
import com.bones.data._
import com.bones.interpreter.validator.{
  KvpInterchangeFormatValidatorInterpreter,
  OptionalInputValidator,
  Validator
}
import reactivemongo.bson.{
  BSONArray,
  BSONBoolean,
  BSONDocument,
  BSONDouble,
  BSONNull,
  BSONString,
  BSONValue
}

/**
  * Module responsible for validating data from BSON and convering to Values.
  */
trait BsonValidatorInterpreter[ALG[_]]
    extends KvpInterchangeFormatValidatorInterpreter[ALG, BSONValue] {

  def generateByteArrayValidator[A](
    schema: KvpCollection[String, ALG, A]
  ): Validator[String, ALG, A, Array[Byte]] = {
    val f = fromKvpCollection(schema)
    (bytes, path) =>
      fromByteArray(bytes).flatMap(f.validateWithPath(_, path))
  }

  /** An additional string in the serialized format which states the coproduct type.
    * TODO:  refactor this interpreter so this property can be overwritten. */
  override val coproductTypeKey: String = "type"

  override def isEmpty(json: BSONValue): Boolean = json match {
    case BSONNull => true
    case _        => false
  }

  type ValidatedFromJsonOption[A] =
    Option[BSONValue] => Either[ExtractionErrors[String], A]
  type ValidatedFromJson[A] =
    BSONValue => Either[ExtractionErrors[String], A]

  override def invalidValue[T](
    bson: BSONValue,
    typeName: String,
    path: List[String]): Left[ExtractionErrors[String], Nothing] = {
    val invalid = bson match {
      case _: BSONBoolean  => classOf[Boolean]
      case _: BSONDouble   => classOf[Double]
      case _: BSONString   => classOf[String]
      case _: BSONArray    => classOf[Array[_]]
      case _: BSONDocument => classOf[Object]
      case _               => classOf[Any]
    }
    Left(List(WrongTypeError(path, typeName, invalid.getSimpleName, None)))
  }

  override def headValue[A](
    in: BSONValue,
    kv: KeyDefinition[String, ALG, A],
    headInterpreter: OptionalInputValidator[String, ALG, A, BSONValue],
    path: List[String]): Either[ExtractionErrors[String], A] = {
    in match {
      case doc: BSONDocument =>
        val fields = doc.elements
        headInterpreter.validateWithPath(fields.find(_.name == kv.key).map(_.value), path)
      case _ => invalidValue(in, kv.typeName, path)
    }

  }

}
