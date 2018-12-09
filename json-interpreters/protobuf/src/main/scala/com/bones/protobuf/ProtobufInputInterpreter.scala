package com.bones.protobuf

import java.io.IOException
import java.time.{Instant, ZoneOffset, ZonedDateTime}
import java.time.format.DateTimeFormatter
import java.util.UUID

import cats.data.NonEmptyList
import com.bones.data.Error.CanNotConvert
import com.bones.data.{Error, KeyValueDefinition, Value}
import com.bones.interpreter.KvpValidateInputInterpreter
import com.google.protobuf.CodedInputStream

object ProtobufInputInterpreter extends KvpValidateInputInterpreter[CodedInputStream] {

  private def convert[A,T](in: CodedInputStream, clazz: Class[A], path: Vector[String])(f: CodedInputStream => A): Either[NonEmptyList[CanNotConvert[CodedInputStream,A]],A] =
    try {
      Right(f(in))
    } catch {
      case e: IOException => Left(NonEmptyList.one(CanNotConvert(path, in, clazz)))
    }


  override def extractString[A](op: Value.ValueDefinitionOp[A], clazz: Class[_])(in: CodedInputStream, path: Vector[String]): Either[NonEmptyList[Error.ExtractionError], String] = {
    convert(in, classOf[String], path)(_.readStringRequireUtf8())
  }


  override def extractLong(op: Value.LongData)(in: CodedInputStream, path: Vector[String]):
    Either[NonEmptyList[Error.ExtractionError], Long] =
      convert(in, classOf[Long], path)(_.readSFixed64())


  override def extractBool(op: Value.BooleanData)(in: CodedInputStream, path: Vector[String]):
    Either[NonEmptyList[Error.ExtractionError], Boolean] =
    convert(in, classOf[Boolean], path)(_.readBool())


  override def extractUuid(op: Value.UuidData)
                          (in: CodedInputStream, path: Vector[String]): Either[NonEmptyList[Error.ExtractionError], UUID] =
    convert(in, classOf[String], path)(_.readString).flatMap(stringToUuid(_,path))


  override def extractZonedDateTime(dateFormat: DateTimeFormatter, op: Value.DateTimeData)(in: CodedInputStream, path: Vector[String]): Either[NonEmptyList[Error.ExtractionError], ZonedDateTime] =
    convert(in, classOf[Long], path)(_.readFixed64())
      .map(epoch => ZonedDateTime.ofInstant(Instant.ofEpochMilli(epoch), ZoneOffset.UTC))

  override def headValue[A](in: CodedInputStream, kv: KeyValueDefinition[A], headInterpreter: (Option[CodedInputStream], Vector[String]) => Either[NonEmptyList[Error.ExtractionError], A], path: Vector[String]): Either[NonEmptyList[Error.ExtractionError], A] = ???

  override def extractArray[A](op: Value.ListData[A])(in: CodedInputStream, path: Vector[String]): Either[NonEmptyList[Error.ExtractionError], Seq[CodedInputStream]] = ???

  override def extractBigDecimal(op: Value.BigDecimalData)(in: CodedInputStream, path: Vector[String]): Either[NonEmptyList[Error.ExtractionError], BigDecimal] = ???

  override def extractXMapArray[A](op: Value.XMapListData[A])(in: CodedInputStream, path: Vector[String]): Either[NonEmptyList[Error.ExtractionError], Seq[CodedInputStream]] = ???

  override protected def invalidValue[T](in: CodedInputStream, expected: Class[T], path: Vector[String]): Left[NonEmptyList[Error.ExtractionError], Nothing] = ???
}
