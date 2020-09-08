package com.bones.bson

import cats.data.NonEmptyList
import cats.implicits._
import com.bones.data.Error._
import com.bones.data._
import com.bones.interpreter.KvpInterchangeFormatValidatorInterpreter
import reactivemongo.bson.buffer.ArrayReadableBuffer
import reactivemongo.bson.{
  BSONArray,
  BSONBoolean,
  BSONDecimal,
  BSONDocument,
  BSONDouble,
  BSONInteger,
  BSONLong,
  BSONNull,
  BSONString,
  BSONValue
}

import scala.util.Try

/**
  * Module responsible for validating data from BSON and convering to Values.
  */
trait BsonValidatorInterpreter[ALG[_]]
    extends KvpInterchangeFormatValidatorInterpreter[ALG, BSONValue] {

  /** An additional string in the serialized format which states the coproduct type.
    * TODO:  refactor this interpreter so this property can be overwritten. */
  override val coproductTypeKey: String = "type"

  override def isEmpty(json: BSONValue): Boolean = json match {
    case BSONNull => true
    case _        => false
  }

  type ValidatedFromJsonOption[A] =
    Option[BSONValue] => Either[NonEmptyList[ExtractionError], A]
  type ValidatedFromJson[A] =
    BSONValue => Either[NonEmptyList[ExtractionError], A]

  override def invalidValue[T](
    bson: BSONValue,
    expected: Class[T],
    path: List[String]): Left[NonEmptyList[ExtractionError], Nothing] = {
    val invalid = bson match {
      case _: BSONBoolean  => classOf[Boolean]
      case _: BSONDouble   => classOf[Double]
      case _: BSONString   => classOf[String]
      case _: BSONArray    => classOf[Array[_]]
      case _: BSONDocument => classOf[Object]
      case _               => classOf[Any]
    }
    Left(NonEmptyList.one(WrongTypeError(path, expected, invalid, None)))
  }

  override def headValue[A](
    in: BSONValue,
    kv: KeyDefinition[ALG, A],
    headInterpreter: (Option[BSONValue], List[String]) => Either[NonEmptyList[ExtractionError], A],
    path: List[String]): Either[NonEmptyList[ExtractionError], A] = {
    in match {
      case doc: BSONDocument =>
        val fields = doc.elements
        headInterpreter(fields.find(_.name == kv.key).map(_.value), path)
      case _ => invalidValue(in, classOf[BSONDocument], path)
    }

  }

}
