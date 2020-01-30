package com.bones.protobuf.custom

import java.time._

import cats.data.NonEmptyList
import com.bones.data.Error.{CanNotConvert, ExtractionError}
import com.bones.data.Sugar
import com.bones.data.custom._
import com.bones.protobuf.ProtobufSequentialInputInterpreter.{
  CustomInterpreter => CustomSequentialInterpreter
}
import com.bones.protobuf.{
  ProtoFileInterpreter,
  ProtobufSequentialInputInterpreter,
  ProtobufSequentialOutputInterpreter
}
import shapeless.HNil

import scala.util.Try

/**
  * This object contains various functions which map data to and from primitive values which
  * are supported by our Coded Input Stream serializer.
  */
object JavaTimeInputInterpreter extends JavaTimeValueSugar with Sugar {

  val stringToDateTimeException
    : (String, List[String]) => Either[NonEmptyList[ExtractionError], DateTimeException] =
    (str, _) => Right(new DateTimeException(str))

  val intToDayOfWeek: (Int, List[String]) => Either[NonEmptyList[ExtractionError], DayOfWeek] =
    (int, _) => Right(DayOfWeek.of(int))

  val stringToDuration: (String, List[String]) => Either[NonEmptyList[ExtractionError], Duration] =
    (str, path) =>
      Try { Duration.parse(str) }.toEither.left
        .map(th => NonEmptyList.one(CanNotConvert(path, str, classOf[Duration], Some(th))))

  val timestampToInstant
    : (Long, Int, List[String]) => Either[NonEmptyList[ExtractionError], Instant] =
    (seconds, nanos, path) =>
      Try { Instant.ofEpochSecond(seconds, nanos) }.toEither.left
        .map(th =>
          NonEmptyList.one(CanNotConvert(path, (seconds, nanos), classOf[Duration], Some(th))))

  val instantToTimestamp: Instant => (Long, Int) =
    instant => (instant.getEpochSecond, instant.getNano)

  val intToMonth: (Int, List[String]) => Either[NonEmptyList[ExtractionError], Month] =
    (int, path) =>
      Try { Month.of(int) }.toEither.left
        .map(th => NonEmptyList.one(CanNotConvert(path, int, classOf[Month], Some(th))))

  /** Since max of month = 12 and max day = 31, we can embed both values in an Int */
  val intToMonthDay: (Int, List[String]) => Either[NonEmptyList[ExtractionError], MonthDay] =
    (int, path) =>
      Try {
        val month = (int >> 16)
        val day = (int & 0xFFFFL).toInt
        MonthDay.of(month, day)
      }.toEither.left.map(th =>
        NonEmptyList.one(CanNotConvert(path, int, classOf[MonthDay], Some(th))))

  /** Month day is encoded into a single int, this function splits them into, essentually, two short values */
  val monthDayToInt: MonthDay => Int =
    monthDay => {
      val month = monthDay.getMonth.getValue
      val day = monthDay.getDayOfMonth
      month << 16 | day.toShort
    }

  val intToZoneOffset: (Int, List[String]) => Either[NonEmptyList[ExtractionError], ZoneOffset] =
    (int, path) =>
      Try { ZoneOffset.ofTotalSeconds(int) }.toEither.left.map(th =>
        NonEmptyList.one(CanNotConvert(path, int, classOf[Month], Some(th))))

  val stringToPeriod: (String, List[String]) => Either[NonEmptyList[ExtractionError], Period] =
    (string, path) =>
      Try { Period.parse(string) }.toEither.left.map(th =>
        NonEmptyList.one(CanNotConvert(path, string, classOf[Period], Some(th))))

  val intToYear: (Int, List[String]) => Either[NonEmptyList[ExtractionError], Year] =
    (int, path) =>
      Try { Year.of(int) }.toEither.left.map(th =>
        NonEmptyList.one(CanNotConvert(path, int, classOf[Period], Some(th))))

  /** Year and Month are encoded as a single long.  This function splits the long into two int values, year/month. */
  val longToYearMonth: (Long, List[String]) => Either[NonEmptyList[ExtractionError], YearMonth] =
    (long, path) =>
      Try {
        val year = (long >> 16).toInt
        val month = (long & 0xFFFFL).toInt
        YearMonth.of(year, month)
      }.toEither.left.map(th =>
        NonEmptyList.one(CanNotConvert(path, long, classOf[YearMonth], Some(th))))

  /** Year and Month are both integers and therefor can be encoded as a single long.  This function does the combining. */
  val yearMonthToLong: YearMonth => Long =
    yearMonth => {
      val year = yearMonth.getYear
      val month = yearMonth.getMonthValue
      year << 16 | month
    }

  val stringToZoneId: (String, List[String]) => Either[NonEmptyList[ExtractionError], ZoneId] =
    (string, path) =>
      Try { ZoneId.of(string) }.toEither.left.map(th =>
        NonEmptyList.one(CanNotConvert(path, string, classOf[ZoneId], Some(th))))

  /** Define an OffsetDateTime in terms of a schema using both natively supported values and JavaTime values
    * supported already.
    */
  val offsetDateTimeSchema =
    (("localDateTime", localDateTime) :<: ("zoneOffset", zoneOffset) :>: kvpNilCov[JavaTimeValue])
      .xmap[OffsetDateTime](
        offsetFields => OffsetDateTime.of(offsetFields.head, offsetFields.tail.head),
        offset => offset.toLocalDateTime :: offset.getOffset :: HNil
      )

  /** Define OffsetTime in terms of a schema using both natvely supported types and JavaTime values
    * supported already.
    */
  val offsetTimeSchema =
    (("localDateTime", localTime) :<: ("zoneOffset", zoneOffset) :>: kvpNilCov[JavaTimeValue])
      .xmap[OffsetTime](
        offsetFields => OffsetTime.of(offsetFields.head, offsetFields.tail.head),
        offset => offset.toLocalTime :: offset.getOffset :: HNil
      )

  /** Define ZonedDateTime in terms of a schema using both natvely supported types and JavaTime values
    * supported already.
    */
  val zonedDateTimeSchema =
    (("localDateTime", localDateTime) :<: ("zoneId", zoneId) :>: kvpNilCov[JavaTimeValue])
      .xmap[ZonedDateTime](
        zonedFields => ZonedDateTime.of(zonedFields.head, zonedFields.tail.head),
        zonedDateTime => zonedDateTime.toLocalDateTime :: zonedDateTime.getZone :: HNil
      )

}

/** Custom interpreter for JavaTimeData */
trait JavaTimeInputInterpreter extends CustomSequentialInterpreter[JavaTimeValue] {

  import com.bones.protobuf.ProtobufSequentialInputInterpreter._
  import JavaTimeInputInterpreter._

  val coreProtobufSequentialInputInterpreter: ProtobufSequentialInputInterpreter

  override def extractFromProto[A](alg: JavaTimeValue[A]): ExtractFromProto[A] =
    alg match {
      case dt: DateTimeExceptionData => stringDataWithFlatMap(Right(dt), stringToDateTimeException)
      case dt: DayOfWeekData         => intDataWithFlatMap(Right(dt), intToDayOfWeek)
      case dd: DurationData          => stringDataWithFlatMap(Right(dd), stringToDuration)
      case id: InstantData           => timestampWithMap(Right(id), timestampToInstant)
      case md: MonthData             => intDataWithFlatMap(Right(md), intToMonth)
      case md: MonthDayData          => intDataWithFlatMap(Right(md), intToMonthDay)
      case dt: OffsetDateTimeData =>
        coreProtobufSequentialInputInterpreter.valueDefinition(offsetDateTimeSchema, this)
      case dt: OffsetTimeData =>
        coreProtobufSequentialInputInterpreter.valueDefinition(offsetTimeSchema, this)
      case pd: PeriodData    => stringDataWithFlatMap(Right(pd), stringToPeriod)
      case yd: YearData      => intDataWithFlatMap(Right(yd), intToYear)
      case ym: YearMonthData => longDataWithFlatMap(Right(ym), longToYearMonth)
      case zd: ZonedDateTimeData =>
        coreProtobufSequentialInputInterpreter.valueDefinition(zonedDateTimeSchema, this)
      case zi: ZoneIdData     => stringDataWithFlatMap(Right(zi), stringToZoneId)
      case zo: ZoneOffsetData => intDataWithFlatMap(Right(zo), intToZoneOffset)
    }

}

trait JavaTimeOutputInterpreter
    extends ProtobufSequentialOutputInterpreter.CustomInterpreter[JavaTimeValue] {
  import ProtobufSequentialOutputInterpreter._
  import JavaTimeInputInterpreter._

  val coreProtobufSequentialOutputInterpreter: ProtobufSequentialOutputInterpreter

  override def encodeToProto[A](alg: JavaTimeValue[A]): EncodeToProto[A] = {
    alg match {
      case dt: DateTimeExceptionData => stringDataFromMap[DateTimeException](_.getMessage)
      case dt: DayOfWeekData         => intDataFromMap[DayOfWeek](_.getValue)
      case dd: DurationData          => stringDataFromMap[Duration](_.toString)
      case id: InstantData           => timestampFromMap(instantToTimestamp)
      case md: MonthData             => intDataFromMap[Month](_.getValue)
      case md: MonthDayData          => intDataFromMap[MonthDay](monthDayToInt)
      case dt: OffsetDateTimeData =>
        coreProtobufSequentialOutputInterpreter.valueDefinition(offsetDateTimeSchema, this)
      case dt: OffsetTimeData =>
        coreProtobufSequentialOutputInterpreter.valueDefinition(offsetTimeSchema, this)
      case pd: PeriodData    => stringDataFromMap[Period](_.toString)
      case yd: YearData      => intDataFromMap[Year](_.getValue)
      case ym: YearMonthData => longDataFromMap(yearMonthToLong)
      case zd: ZonedDateTimeData =>
        coreProtobufSequentialOutputInterpreter.valueDefinition(zonedDateTimeSchema, this)
      case zi: ZoneIdData     => stringDataFromMap(_.toString)
      case zo: ZoneOffsetData => intDataFromMap[ZoneOffset](_.getTotalSeconds)
    }
  }
}

trait JavaTimeProtoFileInterpreter extends ProtoFileInterpreter.CustomInterpreter[JavaTimeValue] {
  import ProtoFileInterpreter._
  import JavaTimeInputInterpreter._

  override def toMessageField[A](alg: JavaTimeValue[A]): (
    Name,
    Int) => (ProtoFileInterpreter.MessageField, Vector[ProtoFileInterpreter.NestedType], Int) =
    (name, index) => {
      alg match {
        case dt: DateTimeExceptionData => stringMessageField(name, index)
        case dt: DayOfWeekData         => intMessageField(name, index)
        case dd: DurationData          => stringMessageField(name, index)
        case id: InstantData           => timestampMessageField(name, index)
        case md: MonthData             => intMessageField(name, index)
        case md: MonthDayData          => intMessageField(name, index)
        case dt: OffsetDateTimeData =>
          ProtoFileInterpreter.valueDefinition(offsetDateTimeSchema, this)(name, index)
        case dt: OffsetTimeData =>
          ProtoFileInterpreter.valueDefinition(offsetTimeSchema, this)(name, index)
        case pd: PeriodData    => stringMessageField(name, index)
        case yd: YearData      => intMessageField(name, index)
        case ym: YearMonthData => longMessageField(name, index)
        case zd: ZonedDateTimeData =>
          ProtoFileInterpreter.valueDefinition(zonedDateTimeSchema, this)(name, index)
        case zi: ZoneIdData     => stringMessageField(name, index)
        case zo: ZoneOffsetData => intMessageField(name, index)
      }
    }
}
