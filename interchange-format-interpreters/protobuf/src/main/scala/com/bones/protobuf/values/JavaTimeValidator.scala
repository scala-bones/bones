package com.bones.protobuf.values

import java.time._

import cats.data.NonEmptyList
import com.bones.Util.{longToLocalDate, longToLocalTime}
import com.bones.data.Error.{CanNotConvert, ExtractionErrors}
import com.bones.data.values._
import com.bones.protobuf.{ExtractFromProto, ProtobufValidatorValue}
import com.bones.validation.ValidationDefinition.ValidationOp
import com.bones.validation.ValidationUtil

import scala.util.Try

object JavaTimeValidator {
  val stringToDateTimeException
    : (String, List[String]) => Either[ExtractionErrors[String], DateTimeException] =
    (str, _) => Right(new DateTimeException(str))

  val intToDayOfWeek: (Int, List[String]) => Either[ExtractionErrors[String], DayOfWeek] =
    (int, _) => Right(DayOfWeek.of(int))

  val stringToDuration: (String, List[String]) => Either[ExtractionErrors[String], Duration] =
    (str, path) =>
      Try { Duration.parse(str) }.toEither.left
        .map(th => NonEmptyList.one(CanNotConvert(path, str, classOf[Duration], Some(th))))

  val timestampToInstant: (Long, Int, List[String]) => Either[ExtractionErrors[String], Instant] =
    (seconds, nanos, path) =>
      Try { Instant.ofEpochSecond(seconds, nanos) }.toEither.left
        .map(th =>
          NonEmptyList.one(CanNotConvert(path, (seconds, nanos), classOf[Duration], Some(th))))

  val instantToTimestamp: Instant => (Long, Int) =
    instant => (instant.getEpochSecond, instant.getNano)

  val intToMonth: (Int, List[String]) => Either[ExtractionErrors[String], Month] =
    (int, path) =>
      Try { Month.of(int) }.toEither.left
        .map(th => NonEmptyList.one(CanNotConvert(path, int, classOf[Month], Some(th))))

  /** Since max of month = 12 and max day = 31, we can embed both values in an Int */
  val intToMonthDay: (Int, List[String]) => Either[ExtractionErrors[String], MonthDay] =
    (int, path) =>
      Try {
        val month = (int >> 16)
        val day = (int & 0xFFFFL).toInt
        MonthDay.of(month, day)
      }.toEither.left.map(th =>
        NonEmptyList.one(CanNotConvert(path, int, classOf[MonthDay], Some(th))))

  /** Month day is encoded into a single int, this function splits them into two short values */
  val monthDayToInt: MonthDay => Int =
    monthDay => {
      val month = monthDay.getMonth.getValue
      val day = monthDay.getDayOfMonth
      month << 16 | day.toShort
    }

  val intToZoneOffset: (Int, List[String]) => Either[ExtractionErrors[String], ZoneOffset] =
    (int, path) =>
      Try { ZoneOffset.ofTotalSeconds(int) }.toEither.left.map(th =>
        NonEmptyList.one(CanNotConvert(path, int, classOf[Month], Some(th))))

  val stringToPeriod: (String, List[String]) => Either[ExtractionErrors[String], Period] =
    (string, path) =>
      Try { Period.parse(string) }.toEither.left.map(th =>
        NonEmptyList.one(CanNotConvert(path, string, classOf[Period], Some(th))))

  val intToYear: (Int, List[String]) => Either[ExtractionErrors[String], Year] =
    (int, path) =>
      Try { Year.of(int) }.toEither.left.map(th =>
        NonEmptyList.one(CanNotConvert(path, int, classOf[Period], Some(th))))

  /** Year and Month are encoded as a single long.  This function splits the long into two int values, year/month. */
  val longToYearMonth: (Long, List[String]) => Either[ExtractionErrors[String], YearMonth] =
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

  val stringToZoneId: (String, List[String]) => Either[ExtractionErrors[String], ZoneId] =
    (string, path) =>
      Try { ZoneId.of(string) }.toEither.left.map(th =>
        NonEmptyList.one(CanNotConvert(path, string, classOf[ZoneId], Some(th))))

}

/** Custom interpreter for JavaTimeData.
  *  Because JavaTimeValidator relies
  *
 **/
trait JavaTimeValidator extends ProtobufValidatorValue[JavaTimeValue] {

  import JavaTimeValidator._

//  def offsetDateTime(lt: OffsetDateTimeData): ExtractFromProto[OffsetDateTime] = {
//
//    val timeAsLongExtract = timestampWithMap[(Long,Int)]( (l,i,_) => Right( (l,i) ), List.empty)
//    val zoneOffsetExtract = intDataWithFlatMap(lt, intToZoneOffset, List.empty)
//
//    (lastFieldNumber, path) => {
//      val ldtResult = timeAsLongExtract(lastFieldNumber, path)
//      val zoneResult = zoneOffsetExtract(ldtResult._2, path)
//
//      val tags = ldtResult._1 ::: zoneResult._1
//      val f: (CanReadTag, CodedInputStream) => (CanReadTag, Either[ExtractionErrors[String], OffsetDateTime]) =
//        (canReadTag, is) => {
//          val ldtReadResult: (CanReadTag, Either[ExtractionErrors[String], (Long, Int)]) =
//            ldtResult._3.apply(canReadTag, is)
//          val zoneReadResult = zoneResult._3(ldtReadResult._1, is)
//          val offsetDateTimeResult = Util.eitherMap2(ldtReadResult._2, zoneReadResult._2)( (ldt, zone) => {
//            OffsetDateTime.of(LocalDateTime.ofEpochSecond(ldt._1, ldt._2, defaultZoneOffset), zone)
//          })
//          val offsetDateTimeValidated = offsetDateTimeResult.flatMap(ValidationUtil.validate(lt.validations))
//          (zoneReadResult._1, offsetDateTimeResult)
//        }
//
//      (tags, zoneResult._2, f)
//    }
//
//  }

  /** Define OffsetTime in terms of a schema using both natively supported types and JavaTime values
    * supported already.
    */
//  val offsetTimeSchema =
//    (("localDateTime", localTime) :: ("zoneOffset", zoneOffset) :: kvpNil)
//      .xmap[OffsetTime](
//        offsetFields => OffsetTime.of(offsetFields.head, offsetFields.tail.head),
//        offset => offset.toLocalTime :: offset.getOffset :: HNil
//      )

  /** Define ZonedDateTime in terms of a schema using both natively supported types and JavaTime values
    * supported already.
    */
//  val zonedDateTimeSchema =
//    (("localDateTime", localDateTime) :: ("zoneId", zoneId) :: kvpNil)
//      .xmap[ZonedDateTime](
//        zonedFields => ZonedDateTime.of(zonedFields.head, zonedFields.tail.head),
//        zonedDateTime => zonedDateTime.toLocalDateTime :: zonedDateTime.getZone :: HNil
//      )

  import com.bones.protobuf.ProtobufSequentialValidatorInterpreter._

  val defaultZoneOffset: ZoneOffset

//  def valueDefThenValidation[ALG[_], A](
//                                 kvpValue: KvpCollection[ALG,A],
//                                 validations: List[ValidationOp[A]]): ExtractFromProto[A] = {
//    (last: LastFieldNumber, path: Path) =>
//      {
//        val (tags, lastField, f) =
//          coreProtobufSequentialInputInterpreter.valueDefinition[ALG, A](kvpValue, this)(last, path)
//        def newF(canRead: CanReadTag, inputStream: CodedInputStream) = {
//          val fResult = f(canRead, inputStream)
//          val newResult2 = fResult._2.flatMap(i => ValidationUtil.validate(validations)(i, path))
//          (fResult._1, newResult2)
//        }
//        (tags, lastField, newF)
//      }
//  }

  def localDateTimeData[ALG[_], A](
    alg: ALG[LocalDateTime],
    zoneOffset: ZoneOffset,
    validations: List[ValidationOp[LocalDateTime]]): ExtractFromProto[LocalDateTime] = {
    def f(
      seconds: Long,
      nanos: Int,
      path: List[String]): Either[ExtractionErrors[String], LocalDateTime] =
      Try {
        LocalDateTime.ofEpochSecond(seconds, nanos, zoneOffset)
      }.toEither.left
        .map(err =>
          NonEmptyList.one(
            CanNotConvert(path, (seconds, nanos), classOf[LocalDateTime], Some(err))))
        .flatMap(i => ValidationUtil.validate(validations)(i, path))

    timestampWithMap(f, validations)
  }

  override def extractFromProto[A](alg: JavaTimeValue[A]): ExtractFromProto[A] =
    alg match {
      case dt: DateTimeExceptionData =>
        stringDataWithFlatMap(dt.typeName, stringToDateTimeException, dt.validations)
      case dt: DayOfWeekData      => intDataWithFlatMap(intToDayOfWeek, dt.validations)
      case dd: DurationData       => stringDataWithFlatMap(dd.typeName, stringToDuration, dd.validations)
      case id: InstantData        => timestampWithMap(timestampToInstant, id.validations)
      case dd: LocalDateTimeData  => localDateTimeData(dd, defaultZoneOffset, dd.validations)
      case dt: LocalDateData      => longDataWithFlatMap(longToLocalDate, dt.validations)
      case lt: LocalTimeData      => longDataWithFlatMap(longToLocalTime, lt.validations)
      case md: MonthData          => intDataWithFlatMap(intToMonth, md.validations)
      case md: MonthDayData       => intDataWithFlatMap(intToMonthDay, md.validations)
      case dt: OffsetDateTimeData => ??? // TODO
//        valueDefThenValidation(offsetDateTimeSchema, dt.validations)
      case dt: OffsetTimeData => ??? // TODO
//        valueDefThenValidation(offsetTimeSchema, dt.validations)
      case pd: PeriodData        => stringDataWithFlatMap(pd.typeName, stringToPeriod, pd.validations)
      case yd: YearData          => intDataWithFlatMap(intToYear, yd.validations)
      case ym: YearMonthData     => longDataWithFlatMap(longToYearMonth, ym.validations)
      case zd: ZonedDateTimeData => ??? // TODO
//        valueDefThenValidation(zonedDateTimeSchema, zd.validations)
      case zi: ZoneIdData     => stringDataWithFlatMap(zi.typeName, stringToZoneId, zi.validations)
      case zo: ZoneOffsetData => intDataWithFlatMap(intToZoneOffset, zo.validations)
    }

}
