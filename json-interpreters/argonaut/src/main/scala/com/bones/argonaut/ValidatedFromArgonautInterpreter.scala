package com.bones.argonaut

import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter
import java.util.{Date, UUID}

import cats.Applicative
import cats.data.Validated.{Invalid, Valid}
import cats.data.{NonEmptyList, Validated}
import com.bones.data.Error.{CanNotConvert, ExtractionError, RequiredData, WrongTypeError}
import com.bones.data.Value._
import cats.implicits._
import shapeless.{HList, HNil, Nat}
import com.bones.validation.{ValidationUtil => vu}
import argonaut._
import Argonaut._
import com.bones.data.KeyValueDefinition
import com.bones.interpreter.KvpValidateInputInterpreter

import scala.util.control.NonFatal

object ValidatedFromArgonautInterpreter extends KvpValidateInputInterpreter[Json] {


  override def headValue[A](in: Json,
                            kv: KeyValueDefinition[A],
                            headInterpreter: (Option[Json], Vector[String]) => Either[NonEmptyList[ExtractionError], A],
                            path: Vector[String]): Either[NonEmptyList[ExtractionError], A] = {

    in.array
      .toRight[NonEmptyList[ExtractionError]](NonEmptyList.one(WrongTypeError(path, classOf[Array[_]], in.getClass)))
      .flatMap(_.find(j => j.name === kv.key).toRight[NonEmptyList[ExtractionError]](NonEmptyList.one(RequiredData(path, kv.op))))
      .flatMap(j => headInterpreter.apply(Some(j), path))
  }


  override def extractString[A](op: ValueDefinitionOp[A], clazz: Class[_])(in: Json, path: Vector[String]):
    Either[NonEmptyList[ExtractionError], String] =
    in.string.toRight(NonEmptyList.one(WrongTypeError(path, clazz, in.getClass)))


  override def extractLong(op: LongData)(in: Json, path: Vector[String]): Either[NonEmptyList[ExtractionError], Long] =
      in.number.flatMap(_.toLong).toRight(NonEmptyList.one(WrongTypeError(path, classOf[Long], in.getClass)))


  override def extractBool(op: BooleanData)(in: Json, path: Vector[String]):
    Either[NonEmptyList[ExtractionError], JsonBoolean] =
      in.bool.toRight(NonEmptyList.one(WrongTypeError(path, classOf[Boolean], in.getClass)))


  override def extractUuid(op: UuidData)(in: Json, path: Vector[String]): Either[NonEmptyList[ExtractionError], UUID] =
    in.string
      .toRight(NonEmptyList.one(WrongTypeError(path, classOf[String], in.getClass)))
      .flatMap(stringToUuid(_,path))


  override def extractZonedDateTime(dateFormat: DateTimeFormatter, op: DateTimeData)
                                   (in: Json, path: Vector[String]):
    Either[NonEmptyList[ExtractionError], ZonedDateTime] =
    in.string
      .toRight(NonEmptyList.one(WrongTypeError(path, classOf[String], in.getClass)))
      .flatMap(stringToZonedDateTime(_,dateFormat, path))


  override def extractArray[A](op: ListData[A])(in: Json, path: Vector[String]):
    Either[NonEmptyList[ExtractionError], Seq[Json]] =
      in.array.toRight(NonEmptyList.one(WrongTypeError(path, classOf[Array[_]],in.getClass)))


  override def extractXMapArray[A](op: XMapListData[A])(in: Json, path: Vector[String]):
    Either[NonEmptyList[ExtractionError], Seq[Json]] =
      in.array.toRight(NonEmptyList.one(WrongTypeError(path, classOf[Array[_]],in.getClass)))


  override def extractBigDecimal(op: BigDecimalData)(in: Json, path: Vector[String]): Either[NonEmptyList[ExtractionError], BigDecimal] =
    in.number.map(_.toBigDecimal).toRight(NonEmptyList.one(WrongTypeError(path, classOf[BigDecimal], in.getClass)))


  override protected def invalidValue[T](in: Json, expected: Class[T], path: Vector[String]): Left[NonEmptyList[ExtractionError], Nothing] = {
    val invalid = in.fold(
      classOf[Nothing],
      _ => classOf[Boolean],
      _ => classOf[Number],
      _ => classOf[String],
      _ => classOf[Array[_]],
      _ => classOf[Object]
    )
    Left(NonEmptyList.one(WrongTypeError(path, expected, invalid)))
  }
}
