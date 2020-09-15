package com.bones.argonaut

import java.nio.charset.Charset

import argonaut.Argonaut._
import argonaut._
import cats.data.NonEmptyList
import cats.syntax.all._
import com.bones.data.Error.{ExtractionError, ParsingError, RequiredValue, WrongTypeError}
import com.bones.data.{KeyDefinition, _}
import com.bones.interpreter.KvpInterchangeFormatValidatorInterpreter

import scala.util.control.NonFatal

/**
  * Module responsible for converting argonaut JSON input into values with validation checks.
  * See [KvpInterchangeFormatValidatorInterpreter.validatorFromSchema] for the entry point.
  */
trait ArgonautValidatorInterpreter[ALG[_]]
    extends KvpInterchangeFormatValidatorInterpreter[ALG, Json] {

  val coproductTypeKey: String

  override def isEmpty(json: Json): JsonBoolean = json.isNull

  /**
    *Creates a function which validates a byte array, convertible to a String with the specified Charset and converts it
    * the type A as defined by the BonesSchema.
    *
    * @param schema The schema, use to create the validation function.
    * @param charset The charset used to convert the Byte Array to a String
    * @tparam A The resulting type -- the type wrapped by the Bones Schema.
    * @return a function validating a Json Byte Array with the specified data.
    */
  def generateByteArrayValidator[A](
    schema: KvpCollection[ALG, A],
    charset: Charset
  ): Array[Byte] => Either[NonEmptyList[ExtractionError], A] = {
    val fromSchemaFunction = fromKvpCollection(schema)
    bytes =>
      {
        try {
          val str = new String(bytes, charset)
          Parse
            .parse(str)
            .left
            .map(str => NonEmptyList.one(ParsingError(str)))
            .flatMap(fromSchemaFunction(_, List.empty))
        } catch {
          case NonFatal(ex) =>
            Left(NonEmptyList.one(ParsingError(ex.getMessage, Some(ex))))
        }
      }
  }

  override def headValue[A](
    in: Json,
    kv: KeyDefinition[ALG, A],
    headInterpreter: (Option[Json], List[String]) => Either[NonEmptyList[ExtractionError], A],
    path: List[String]
  ): Either[NonEmptyList[ExtractionError], A] = {

    in.obj
      .toRight[NonEmptyList[ExtractionError]](
        NonEmptyList.one(WrongTypeError(path, kv.typeName, in.getClass.getSimpleName, None))
      )
      .flatMap(
        _.toList
          .find(f => f._1 === kv.key)
          .toRight[NonEmptyList[ExtractionError]](
            NonEmptyList.one(RequiredValue(path, kv.typeName))
          )
      )
      .flatMap(j => headInterpreter.apply(Some(j._2), path))
  }

  override def invalidValue[T](
    in: Json,
    typeName: String,
    path: List[String]
  ): Left[NonEmptyList[ExtractionError], Nothing] = {
    val invalid = in.fold(
      classOf[Nothing],
      _ => classOf[Boolean],
      _ => classOf[Number],
      _ => classOf[String],
      _ => classOf[Array[_]],
      _ => classOf[Object]
    )
    Left(NonEmptyList.one(WrongTypeError(path, typeName, invalid.getSimpleName, None)))
  }

}
