package com.bones.argonaut

import java.nio.charset.Charset

import argonaut.Argonaut._
import argonaut._
import com.bones.data.Error.{ExtractionErrors, ParsingError, RequiredValue, WrongTypeError}
import com.bones.data.{KeyDefinition, _}
import com.bones.interpreter.OptionalInputValidator
import com.bones.interpreter.validator.{
  KvpInterchangeFormatValidatorInterpreter,
  OptionalInputValidator
}

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
    schema: KvpCollection[String, ALG, A],
    charset: Charset
  ): Array[Byte] => Either[ExtractionErrors[String], A] = {
    val fromSchemaFunction = fromKvpCollection(schema)
    bytes =>
      {
        try {
          val str = new String(bytes, charset)
          Parse
            .parse(str)
            .left
            .map(str => List(ParsingError[String](str)))
            .flatMap(fromSchemaFunction.validate)
        } catch {
          case NonFatal(ex) =>
            Left(List(ParsingError(ex.getMessage, Some(ex))))
        }
      }
  }

  override def headValue[A](
    in: Json,
    kv: KeyDefinition[String, ALG, A],
    headInterpreter: OptionalInputValidator[String, ALG, A, Json],
    path: List[String]
  ): Either[ExtractionErrors[String], A] = {

    in.obj
      .toRight[ExtractionErrors[String]](
        List(WrongTypeError(path, kv.typeName, in.getClass.getSimpleName, None))
      )
      .flatMap(
        _.toList
          .find(f => f._1 == kv.key)
          .toRight[ExtractionErrors[String]](
            List(RequiredValue(path, kv.typeName))
          )
      )
      .flatMap(j => headInterpreter.validateWithPath(Some(j._2), path))
  }

  override def invalidValue[T](
    in: Json,
    typeName: String,
    path: List[String]
  ): Left[ExtractionErrors[String], Nothing] = {
    val invalid = in.fold(
      classOf[Nothing],
      _ => classOf[Boolean],
      _ => classOf[Number],
      _ => classOf[String],
      _ => classOf[Array[_]],
      _ => classOf[Object]
    )
    Left(List(WrongTypeError(path, typeName, invalid.getSimpleName, None)))
  }

}
