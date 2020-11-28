package com.bones.data.values

import java.time._
import java.time.format.DateTimeFormatter

import com.bones.PrimitiveValueManifestTypeName
import com.bones.validation.ValidationDefinition.ValidationOp
import com.bones.validation.values.JavaTimeValidation
import com.bones.validation.values.JavaTimeValidation._
import shapeless.Coproduct
import shapeless.ops.coproduct.Inject // because scala 2.12
sealed abstract class JavaTimeValue[A: Manifest] extends PrimitiveValueManifestTypeName[A] {
  val manifestOfA: Manifest[A] = manifest[A]
  val validations: List[ValidationOp[A]]
}

/** Note that Deserializing DateTimeException, although support throughout, will contain a misleading stack trace.  This is because
  * it is impossible to create a new DateTimeException without suppressing the stack trace.  This class is type
  * is mainly here for Serialization. */
final case class DateTimeExceptionData(validations: List[ValidationOp[DateTimeException]])
    extends JavaTimeValue[DateTimeException]

final case class DayOfWeekData(validations: List[ValidationOp[DayOfWeek]])
    extends JavaTimeValue[DayOfWeek]

final case class DurationData(validations: List[ValidationOp[Duration]])
    extends JavaTimeValue[Duration]

final case class InstantData(validations: List[ValidationOp[Instant]])
    extends JavaTimeValue[Instant]

final case class LocalDateTimeData(validations: List[ValidationOp[LocalDateTime]])
    extends JavaTimeValue[LocalDateTime]

final case class LocalDateData(validations: List[ValidationOp[LocalDate]])
    extends JavaTimeValue[LocalDate]

final case class LocalTimeData(validations: List[ValidationOp[LocalTime]])
    extends JavaTimeValue[LocalTime]

final case class MonthData(validations: List[ValidationOp[Month]]) extends JavaTimeValue[Month]

final case class MonthDayData(validations: List[ValidationOp[MonthDay]])
    extends JavaTimeValue[MonthDay]

final case class OffsetDateTimeData(validations: List[ValidationOp[OffsetDateTime]])
    extends JavaTimeValue[OffsetDateTime]

final case class OffsetTimeData(validations: List[ValidationOp[OffsetTime]])
    extends JavaTimeValue[OffsetTime]

final case class PeriodData(validations: List[ValidationOp[Period]]) extends JavaTimeValue[Period]

final case class YearData(validations: List[ValidationOp[Year]]) extends JavaTimeValue[Year]

final case class YearMonthData(validations: List[ValidationOp[YearMonth]])
    extends JavaTimeValue[YearMonth]

final case class ZonedDateTimeData(validations: List[ValidationOp[ZonedDateTime]])
    extends JavaTimeValue[ZonedDateTime]

final case class ZoneIdData(validations: List[ValidationOp[ZoneId]]) extends JavaTimeValue[ZoneId]

final case class ZoneOffsetData(validations: List[ValidationOp[ZoneOffset]])
    extends JavaTimeValue[ZoneOffset]

trait JavaTimeValidationSugar {

  val jt_dow: JavaTimeValidation.DayOfWeekValidation.type = DayOfWeekValidation
  val jt_d: JavaTimeValidation.DurationValidation.type = DurationValidation
  val jt_i: JavaTimeValidation.InstantValidation.type = InstantValidation
  val jt_m: JavaTimeValidation.MonthValidations.type = MonthValidations
  val jt_md: JavaTimeValidation.MonthDayValidations.type = MonthDayValidations
  val jt_ldt: JavaTimeValidation.LocalDateTimeValidation.type = LocalDateTimeValidation
  val jt_ld: JavaTimeValidation.LocalDateValidation.type = LocalDateValidation
  val jt_lt: JavaTimeValidation.LocalTimeValidation.type = LocalTimeValidation
  val jt_odt: JavaTimeValidation.OffsetDateTimeValidations.type = OffsetDateTimeValidations
  val jt_ot: JavaTimeValidation.OffsetTimeValidations.type = OffsetTimeValidations
  val jt_p: JavaTimeValidation.PeriodValidations.type = PeriodValidations
  val jt_y: JavaTimeValidation.YearValidations.type = YearValidations
  val jt_ym: JavaTimeValidation.YearMonthValidations.type = YearMonthValidations
  val jt_zdt: JavaTimeValidation.ZonedDateTimeValidations.type = ZonedDateTimeValidations
  val jt_zi: JavaTimeValidation.ZoneIdValidations.type = ZoneIdValidations
  val jt_zo: JavaTimeValidation.ZoneOffsetValidations.type = ZoneOffsetValidations

}

trait BaseJavaTimeInterpreter[OUT] {
  def matchJavaTimeValue[A](alg: JavaTimeValue[A]): OUT = {
    alg match {
      case dd: DateTimeExceptionData => dateTimeExceptionData(dd)
      case dd: DayOfWeekData         => dayOfWeekData(dd)
      case dd: DurationData          => durationData(dd)
      case id: InstantData           => instantData(id)
      case ld: LocalDateTimeData     => localDateTimeData(ld)
      case ld: LocalDateData         => localDateData(ld)
      case lt: LocalTimeData         => localTimeData(lt)
      case md: MonthData             => monthData(md)
      case md: MonthDayData          => monthDayData(md)
      case od: OffsetDateTimeData    => offsetDateTimeData(od)
      case od: OffsetTimeData        => offsetTimeData(od)
      case pd: PeriodData            => periodData(pd)
      case yd: YearData              => yearData(yd)
      case yd: YearMonthData         => yearMonthData(yd)
      case zd: ZonedDateTimeData     => zonedDateTimeData(zd)
      case zd: ZoneIdData            => zoneIdData(zd)
      case zd: ZoneOffsetData        => zoneOffsetData(zd)
    }
  }

  def dateTimeExceptionData(dateTimeExceptionData: DateTimeExceptionData): OUT
  def dayOfWeekData(dayOfWeekData: DayOfWeekData): OUT
  def durationData(durationData: DurationData): OUT
  def instantData(instantData: InstantData): OUT
  def localDateTimeData(localDateTimeData: LocalDateTimeData): OUT
  def localDateData(localDateData: LocalDateData): OUT
  def localTimeData(localTimeData: LocalTimeData): OUT
  def monthData(monthData: MonthData): OUT
  def monthDayData(monthDayData: MonthDayData): OUT
  def offsetDateTimeData(offsetDateTimeData: OffsetDateTimeData): OUT
  def offsetTimeData(offsetTimeData: OffsetTimeData): OUT
  def periodData(periodData: PeriodData): OUT
  def yearData(yearData: YearData): OUT
  def yearMonthData(yearMonthData: YearMonthData): OUT
  def zonedDateTimeData(zonedDateTimeData: ZonedDateTimeData): OUT
  def zoneIdData(zoneIdData: ZoneIdData): OUT
  def zoneOffsetData(zoneOffsetData: ZoneOffsetData): OUT

}

/** Convenient methods for creating GADTs when this is the only data type being used.  If using multiple custom
  * algebras, use [[JavaTimeValueSugarInjected]] to inject into a Coproduct context. */
trait JavaTimeValueSugar extends JavaTimeValidationSugar {
  def dateTimeException(validations: ValidationOp[DateTimeException]*): DateTimeExceptionData =
    DateTimeExceptionData(validations.toList)

  def dateTimeException: DateTimeExceptionData = dateTimeException()

  def dayOfWeek(validations: ValidationOp[DayOfWeek]*): DayOfWeekData =
    DayOfWeekData(validations.toList)
  def dayOfWeek: DayOfWeekData = dayOfWeek()

  def duration(validations: ValidationOp[Duration]*): DurationData =
    DurationData(validations.toList)
  val duration: DurationData = duration()

  def instant(validations: ValidationOp[Instant]*): InstantData = InstantData(validations.toList)
  def instant: InstantData = instant()

  /** Indicates that the data tied to this key is a Date type with the specified format that must pass the specified validations. */
  def localDateTime(v: ValidationOp[LocalDateTime]*): LocalDateTimeData =
    LocalDateTimeData(v.toList)

  def localDateTime: LocalDateTimeData = LocalDateTimeData(List.empty)

  def localDate(v: ValidationOp[LocalDate]*): LocalDateData = LocalDateData(v.toList)

  def localDate: LocalDateData = LocalDateData(List.empty)

  def localTime(v: ValidationOp[LocalTime]*): LocalTimeData = LocalTimeData(v.toList)

  def localTime: LocalTimeData = localTime()

  def month(validations: ValidationOp[Month]*): MonthData = MonthData(validations.toList)
  def month: MonthData = month()

  def monthDay(validations: ValidationOp[MonthDay]*): MonthDayData =
    MonthDayData(validations.toList)
  def monthDay: MonthDayData = monthDay()

  def offsetDateTime(validations: ValidationOp[OffsetDateTime]*): OffsetDateTimeData =
    OffsetDateTimeData(validations.toList)
  def offsetDateTime: OffsetDateTimeData = offsetDateTime()

  def offsetTime(validations: ValidationOp[OffsetTime]*): OffsetTimeData =
    OffsetTimeData(validations.toList)
  def offsetTime: OffsetTimeData = offsetTime()

  def period(validations: ValidationOp[Period]*): PeriodData = PeriodData(validations.toList)
  def period: PeriodData = period()

  def year(validations: ValidationOp[Year]*): YearData = YearData(validations.toList)
  def year: YearData = year()

  def yearMonth(validations: ValidationOp[YearMonth]*): YearMonthData =
    YearMonthData(validations.toList)
  def yearMonth: YearMonthData = yearMonth()

  def zonedDateTime(validations: ValidationOp[ZonedDateTime]*): ZonedDateTimeData =
    ZonedDateTimeData(validations.toList)
  def zonedDateTime: ZonedDateTimeData = zonedDateTime()

  def zoneId(validations: ValidationOp[ZoneId]*): ZoneIdData = ZoneIdData(validations.toList)
  def zoneId: ZoneIdData = zoneId()

  def zoneOffset(validations: ValidationOp[ZoneOffset]*): ZoneOffsetData =
    ZoneOffsetData(validations.toList)
  def zoneOffset: ZoneOffsetData = zoneOffset()
}

/** Adds smart constructors to lift our GADT into a multi-algebra coproduct */
trait JavaTimeValueSugarInjected[ALG[_] <: Coproduct] extends JavaTimeValidationSugar {

  def javaTimeInject[A]: Inject[ALG[A], JavaTimeValue[A]]

  def dateTimeException(validations: ValidationOp[DateTimeException]*): ALG[DateTimeException] =
    javaTimeInject(DateTimeExceptionData(validations.toList))

  def dateTimeException: ALG[DateTimeException] = dateTimeException()

  def dayOfWeek(validations: ValidationOp[DayOfWeek]*): ALG[DayOfWeek] =
    javaTimeInject[DayOfWeek].apply(DayOfWeekData(validations.toList))

  def dayOfWeek: ALG[DayOfWeek] = dayOfWeek()

  def duration(validations: ValidationOp[Duration]*): ALG[Duration] =
    javaTimeInject(DurationData(validations.toList))
  def duration: ALG[Duration] = duration()

  def instant(validations: ValidationOp[Instant]*): ALG[Instant] =
    javaTimeInject(InstantData(validations.toList))
  def instant: ALG[Instant] = instant()

  /** Indicates that the data tied to this key is a Date type with the specified format that must pass the specified validations. */
  def localDateTime(v: ValidationOp[LocalDateTime]*): ALG[LocalDateTime] =
    javaTimeInject(LocalDateTimeData(v.toList))

  def localDateTime: ALG[LocalDateTime] = localDateTime()

  def localDate(v: ValidationOp[LocalDate]*): ALG[LocalDate] =
    javaTimeInject(LocalDateData(v.toList))

  def localDate: ALG[LocalDate] = localDate()

  def localTime(v: ValidationOp[LocalTime]*): ALG[LocalTime] =
    javaTimeInject(LocalTimeData(v.toList))

  def localTime: ALG[LocalTime] = localTime()

  def month(validations: ValidationOp[Month]*): ALG[Month] =
    javaTimeInject(MonthData(validations.toList))
  def month: ALG[Month] = month()

  def monthDay(validations: ValidationOp[MonthDay]*): ALG[MonthDay] =
    javaTimeInject(MonthDayData(validations.toList))
  def monthDay: ALG[MonthDay] = monthDay()

  def offsetDateTime(validations: ValidationOp[OffsetDateTime]*): ALG[OffsetDateTime] =
    javaTimeInject(OffsetDateTimeData(validations.toList))
  def offsetDateTime: ALG[OffsetDateTime] = offsetDateTime()

  def offsetTime(validations: ValidationOp[OffsetTime]*): ALG[OffsetTime] =
    javaTimeInject(OffsetTimeData(validations.toList))
  def offsetTime: ALG[OffsetTime] = offsetTime()

  def period(validations: ValidationOp[Period]*): ALG[Period] =
    javaTimeInject(PeriodData(validations.toList))
  def period: ALG[Period] = period()

  def year(validations: ValidationOp[Year]*): ALG[Year] =
    javaTimeInject(YearData(validations.toList))
  def year: ALG[Year] = year()

  def yearMonth(validations: ValidationOp[YearMonth]*): ALG[YearMonth] =
    javaTimeInject(YearMonthData(validations.toList))
  def yearMonth: ALG[YearMonth] = yearMonth()

  def zonedDateTime(validations: ValidationOp[ZonedDateTime]*): ALG[ZonedDateTime] =
    javaTimeInject(ZonedDateTimeData(validations.toList))
  def zonedDateTime: ALG[ZonedDateTime] = zonedDateTime()

  def zoneId(validations: ValidationOp[ZoneId]*): ALG[ZoneId] =
    javaTimeInject(ZoneIdData(validations.toList))
  def zoneId: ALG[ZoneId] = zoneId()

  def zoneOffset(validations: ValidationOp[ZoneOffset]*): ALG[ZoneOffset] =
    javaTimeInject(ZoneOffsetData(validations.toList))

  def zoneOffset: ALG[ZoneOffset] = zoneOffset()
}

object JavaTimeValueDefaultJsonMetadata {

  val instantExample = Instant.ofEpochSecond(1581349194)
  val offsetDateTimeExample = OffsetDateTime.ofInstant(instantExample, ZoneId.of("Z"))
  val periodExample = Period.ofMonths(3)
  val localDateExample = LocalDate.of(1970, 1, 1)
  val localTimeExample = LocalTime.of(12, 0, 0, 0)
  val localDateTimeExample = LocalDateTime.of(localDateExample, localTimeExample)

  def getDefaultDescription[A](alg: JavaTimeValue[A]): String = {
    alg match {
      case dte: DateTimeExceptionData =>
        "Value representing an Error With Date Time"
      case dow: DayOfWeekData =>
        "day of the week"
      case d: DurationData =>
        "duration"
      case id: InstantData =>
        "instant"
      case ldt: LocalDateTimeData =>
        "local date time"
      case ld: LocalDateData =>
        "local date"
      case lt: LocalTimeData =>
        "local time"
      case md: MonthData =>
        "month of year"
      case md: MonthDayData =>
        "month/day"
      case od: OffsetDateTimeData =>
        "offset date/time"
      case od: OffsetTimeData =>
        "offset time"
      case pd: PeriodData =>
        "time period"
      case yd: YearData =>
        "year"
      case ym: YearMonthData =>
        "year/month"
      case z: ZoneIdData =>
        "zone id offset"
      case z: ZonedDateTimeData =>
        "zoned date time"
      case z: ZoneOffsetData =>
        "zone offset"
    }

  }

  def getDefaultExample[A](alg: JavaTimeValue[A]): A = {
    val example = alg match {
      case dte: DateTimeExceptionData =>
        new DateTimeException("Example Date Time Exception Message")
      case dow: DayOfWeekData =>
        DayOfWeek.FRIDAY
      case d: DurationData =>
        Duration.ofHours(24)
      case id: InstantData =>
        instantExample
      case ldt: LocalDateTimeData =>
        localDateTimeExample
      case ld: LocalDateData =>
        localDateExample
      case lt: LocalTimeData =>
        localTimeExample
      case md: MonthData =>
        Month.JANUARY
      case md: MonthDayData =>
        MonthDay.of(Month.JANUARY, 1)
      case od: OffsetDateTimeData =>
        offsetDateTimeExample
      case od: OffsetTimeData =>
        offsetDateTimeExample.toOffsetTime
      case pd: PeriodData =>
        periodExample
      case yd: YearData =>
        Year.of(2020)
      case ym: YearMonthData =>
        YearMonth.of(2020, Month.JANUARY)
      case z: ZoneIdData =>
        ZoneId.systemDefault
      case z: ZonedDateTimeData =>
        instantExample
      case z: ZoneOffsetData =>
        ZoneOffset.UTC
    }
    example.asInstanceOf[A]
  }
}
