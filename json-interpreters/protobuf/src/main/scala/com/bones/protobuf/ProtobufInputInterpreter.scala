package com.bones.protobuf

import java.io.IOException
import java.time.{Instant, ZoneOffset, ZonedDateTime}
import java.time.format.DateTimeFormatter
import java.util.UUID

import cats.data.NonEmptyList
import com.bones.data.Error.{CanNotConvert, ExtractionError}
import com.bones.data.Value._
import com.bones.data.{Error, KeyValueDefinition, Value}
import com.bones.interpreter.KvpValidateInputInterpreter._
import com.google.protobuf.CodedInputStream
import shapeless.{HList, Nat}

object ProtobufInputInterpreter {

  type Path = Vector[String]

  def dataClass[A](dc: DataClass[A]): (CodedInputStream, Path) => Either[NonEmptyList[ExtractionError],A] = {
    dc match {
      case t: XMapData[a, al, b] => ???
      case o: OptionalDataClass[a] => ???
      case ld: XMapListData[b] => ???
    }
  }

  type Tag = Int

  def kvpGroup[H<:HList,HL<:Nat](group: KvpGroup[H,HL]): (CodedInputStream, Path) => Either[NonEmptyList[ExtractionError],H] = {
    group match {
      case KvpNil => ???
      case op: KvpSingleValueHead[h, t, tl, a] => ???
      case op: KvpGroupHead[a, al, h, hl, t, tl] => ???
      case op: KvpDataClassHead[h,t,tl,out] => ???
      case op: OptionalKvpGroup[h,hl] => ???
    }
  }

  def valueDefinition[A](fgo: ValueDefinitionOp[A]): (CodedInputStream, Path) => Either[NonEmptyList[ExtractionError],A] =
    fgo match {
      case op: OptionalValueDefinition[a] =>
        (in, path) => valueDefinition(op.valueDefinitionOp)(in,path).map(Option(_))
      case ob: BooleanData =>
        (in, path) => convert(in, classOf[Boolean], path)(_.readBool())
      case rs: StringData =>
        (in, path) => convert(in, classOf[String], path)(_.readStringRequireUtf8())
      case ri: LongData =>
        (in, path) => convert(in, classOf[Long], path)(_.readSFixed64())
      case uu: UuidData =>
        (in, path) => convert(in, classOf[String], path)(_.readString).flatMap(stringToUuid(_,path))
      case dd: DateTimeData =>
        (in, path) => convert(in, classOf[Long], path)(_.readFixed64())
          .map(epoch => ZonedDateTime.ofInstant(Instant.ofEpochMilli(epoch), ZoneOffset.UTC))
      case bd: BigDecimalData =>
        (in, path) => convert(in, classOf[String], path)(_.readString()).flatMap(stringToBigDecimal(_,path))
      case ld: ListData[t] => ???
      case ed: EitherData[a,b] => ???
      case esd: EnumerationStringData[a] =>
        (in, path) => convert(in, classOf[String], path)(_.readString()).flatMap(stringToEnumeration(_, path, esd.enumeration, esd.manifestOfA)).map(_.asInstanceOf[A])
      case esd: EnumStringData[a] => ???
        (in, path) => convert(in, classOf[String], path)(_.readString()).flatMap(stringToEnum[a](_, path, esd.enums)).map(_.asInstanceOf[A])
      case kvp: KvpGroupData[h,hl] => ???
    }

  private def convert[A,T](in: CodedInputStream, clazz: Class[A], path: Vector[String])(f: CodedInputStream => A): Either[NonEmptyList[CanNotConvert[CodedInputStream,A]],A] =
    try {
      Right(f(in))
    } catch {
      case e: IOException => Left(NonEmptyList.one(CanNotConvert(path, in, clazz)))
    }

}

