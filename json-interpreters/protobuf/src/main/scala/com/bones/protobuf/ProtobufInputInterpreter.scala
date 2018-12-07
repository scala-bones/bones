package com.bones.protobuf

import java.io.IOException
import java.time.{Instant, ZoneOffset, ZonedDateTime}
import java.time.format.DateTimeFormatter
import java.util.UUID

import cats.data.NonEmptyList
import com.bones.data.Error.CanNotConvert
import com.bones.data.{Error, KeyValueDefinition}
import com.bones.interpreter.KvpValidateInputInterpreter
import com.google.protobuf.CodedInputStream

object ProtobufInputInterpreter extends KvpValidateInputInterpreter[CodedInputStream] {

  private def convert[A,T](in: CodedInputStream, clazz: Class[A])(f: CodedInputStream => A): Either[NonEmptyList[CanNotConvert[CodedInputStream,A]],A] =
    try {
      Right(f(in))
    } catch {
      case e: IOException => Left(NonEmptyList.one(CanNotConvert(in, clazz)))
    }

  override def headValue[A](in: CodedInputStream, kv: KeyValueDefinition[A], headInterpreter: Option[CodedInputStream] => Either[NonEmptyList[Error.ExtractionError], A]): Either[NonEmptyList[Error.ExtractionError], A] = ???

  override def extractString(in: CodedInputStream): Either[NonEmptyList[Error.ExtractionError], String] = {
    convert(in, classOf[String])(_.readStringRequireUtf8())
  }

  override def extractLong(in: CodedInputStream): Either[NonEmptyList[Error.ExtractionError], Long] =
    convert(in, classOf[Long])(_.readSFixed64())

  override def extractBool(in: CodedInputStream): Either[NonEmptyList[Error.ExtractionError], Boolean] =
    convert(in, classOf[Boolean])(_.readBool())

  override def extractUuid(in: CodedInputStream): Either[NonEmptyList[Error.ExtractionError], UUID] =
    convert(in, classOf[String])(_.readString).flatMap(stringToUuid)

  override def extractZonedDateTime(in: CodedInputStream, dateFormat: DateTimeFormatter): Either[NonEmptyList[Error.ExtractionError], ZonedDateTime] =
    convert(in, classOf[Long])(_.readFixed64())
      .map(epoch => ZonedDateTime.ofInstant(Instant.ofEpochMilli(epoch), ZoneOffset.UTC))

  override def extractArray(in: CodedInputStream): Either[NonEmptyList[Error.ExtractionError], Seq[CodedInputStream]] = ???

  override def extractBigDecimal(in: CodedInputStream): Either[NonEmptyList[Error.ExtractionError], BigDecimal] = ???

  override protected def invalidValue[T](in: CodedInputStream, expected: Class[T]): Left[NonEmptyList[Error.ExtractionError], Nothing] = ???
}
