package com.bones.protobuf.custom

import java.time._

import cats.data.NonEmptyList
import com.bones.data.Error.{CanNotConvert, ExtractionError}
import com.bones.data.{HListConvert, KvpValue, Sugar}
import com.bones.data.custom._
import com.bones.protobuf.ProtobufSequentialValidatorInterpreter.{ExtractFromProto, LastFieldNumber, Path, CustomValidatorInterpreter => CustomSequentialInterpreter}
import com.bones.protobuf.{ProtoFileGeneratorInterpreter, ProtobufSequentialEncoderInterpreter, ProtobufSequentialValidatorInterpreter}
import com.bones.syntax.NoAlgebra
import com.bones.validation.ValidationDefinition.ValidationOp
import com.bones.validation.ValidationUtil
import com.google.protobuf.CodedInputStream
import shapeless.HNil

import scala.util.Try

/**
  * This object contains various functions which map data to and from primitive values which
  * are supported by our Coded Input Stream serializer.
  */
object JavaTimeValidatorInterpreter extends JavaTimeValueSugar with Sugar {

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
trait JavaTimeValidatorInterpreter extends CustomSequentialInterpreter[JavaTimeValue] {

  import com.bones.protobuf.ProtobufSequentialValidatorInterpreter._
  import JavaTimeValidatorInterpreter._

  val coreProtobufSequentialInputInterpreter: ProtobufSequentialValidatorInterpreter

  def valueDefThenValidation[A](kvpValue: KvpValue[A], validations: List[ValidationOp[A]]): ExtractFromProto[A] = {
    (last: LastFieldNumber, path: Path) => {
      val (tags, lastField, f) = coreProtobufSequentialInputInterpreter.valueDefinition(kvpValue, this)(last, path)
      def newF(canRead: CanReadTag, inputStream: CodedInputStream) = {
        val fResult = f(canRead, inputStream)
        val newResult2 = fResult._2.flatMap(i => ValidationUtil.validate(validations)(i, path))
        (fResult._1, newResult2)
      }
      (tags, lastField, newF)
    }
  }


  override def extractFromProto[A](alg: JavaTimeValue[A]): ExtractFromProto[A] =
    alg match {
      case dt: DateTimeExceptionData => stringDataWithFlatMap(Right(dt), stringToDateTimeException, dt.validations)
      case dt: DayOfWeekData         => intDataWithFlatMap(Right(dt), intToDayOfWeek, dt.validations)
      case dd: DurationData          => stringDataWithFlatMap(Right(dd), stringToDuration, dd.validations)
      case id: InstantData           => timestampWithMap(Right(id), timestampToInstant, id.validations)
      case md: MonthData             => intDataWithFlatMap(Right(md), intToMonth, md.validations)
      case md: MonthDayData          => intDataWithFlatMap(Right(md), intToMonthDay, md.validations)
      case dt: OffsetDateTimeData =>
          valueDefThenValidation(offsetDateTimeSchema, dt.validations)
      case dt: OffsetTimeData =>
        valueDefThenValidation(offsetTimeSchema, dt.validations)
      case pd: PeriodData    => stringDataWithFlatMap(Right(pd), stringToPeriod, pd.validations)
      case yd: YearData      => intDataWithFlatMap(Right(yd), intToYear, yd.validations)
      case ym: YearMonthData => longDataWithFlatMap(Right(ym), longToYearMonth, ym.validations)
      case zd: ZonedDateTimeData =>
        valueDefThenValidation(zonedDateTimeSchema, zd.validations)
      case zi: ZoneIdData     => stringDataWithFlatMap(Right(zi), stringToZoneId, zi.validations)
      case zo: ZoneOffsetData => intDataWithFlatMap(Right(zo), intToZoneOffset, zo.validations)
    }

}


