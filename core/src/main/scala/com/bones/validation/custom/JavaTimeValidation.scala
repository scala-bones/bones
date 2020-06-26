package com.bones.validation.custom

import java.time.format.DateTimeFormatter
import java.time._

import com.bones.validation.ValidationDefinition._

object JavaTimeValidation {

  object DayOfWeekValidation extends BaseValidationOp[DayOfWeek]

  object DurationValidation
      extends BaseValidationOp[Duration]
      with OrderingValidation[Duration]
      with ZeroValidations[Duration] {

    /** What is the zero Value of N.  Used for positive and negative comparisons */
    override val zero: Duration = Duration.ZERO

    override def compare(x: Duration, y: Duration): Int = x.compareTo(y)
  }

  object InstantValidation extends BaseDateValidation[Instant] with Ordering[Instant] {

    override def defaultFormatToString(f: Instant): String = DateTimeFormatter.ISO_INSTANT.format(f)

    /** Used in the error string to describe the type.  For instance: 'date' */
    override val instantDescription: String = "instant"

    override def compare(x: Instant, y: Instant): Int = x.compareTo(y)
  }

  object LocalDateTimeValidation
      extends BaseDateValidation[LocalDateTime]
      with Ordering[LocalDateTime] {
    override def defaultFormatToString(f: LocalDateTime): String =
      DateTimeFormatter.ISO_LOCAL_DATE_TIME.format(f)

    override val instantDescription: String = "date/time"

    override def compare(x: LocalDateTime, y: LocalDateTime): Int = x.compareTo(y)
  }

  object LocalTimeValidation
      extends BaseDateValidation[LocalTime]
      with Ordering[LocalTime] {
    override def defaultFormatToString(f: LocalTime): String =
      DateTimeFormatter.ISO_LOCAL_TIME.format(f)

    /** Used in the error string to describe the type.  For instance: 'date' */
    override val instantDescription: String = "time"

    override def compare(x: LocalTime, y: LocalTime): Int = x.compareTo(y)
  }

  object LocalDateValidation
      extends BaseDateValidation[LocalDate]
      with Ordering[LocalDate] {

    override def defaultFormatToString(f: LocalDate): String =
      DateTimeFormatter.ISO_LOCAL_DATE.format(f)

    /** Used in the error string to describe the type.  For instance: 'date' */
    override val instantDescription: String = "date"

    override def compare(x: LocalDate, y: LocalDate): Int = x.compareTo(y)
  }

  object MonthValidations extends BaseValidationOp[Month]

  object MonthDayValidations extends BaseDateValidation[MonthDay] {

    override def defaultFormatToString(f: MonthDay): String = f.toString

    /** Used in the error string to describe the type.  For instance: 'date' */
    override val instantDescription: String = "month/day"

    override def compare(x: MonthDay, y: MonthDay): Int = x.compareTo(y)
  }

  object OffsetDateTimeValidations extends BaseDateValidation[OffsetDateTime] {
    override def defaultFormatToString(f: OffsetDateTime): String =
      DateTimeFormatter.ISO_OFFSET_DATE_TIME.format(f)

    /** Used in the error string to describe the type.  For instance: 'date' */
    override val instantDescription: String = "offset date/time"

    override def compare(x: OffsetDateTime, y: OffsetDateTime): Int = x.compareTo(y)
  }

  object OffsetTimeValidations extends BaseDateValidation[OffsetTime] {
    override def defaultFormatToString(f: OffsetTime): String =
      DateTimeFormatter.ISO_OFFSET_TIME.format(f)

    /** Used in the error string to describe the type.  For instance: 'date' */
    override val instantDescription: String = "offset time"

    override def compare(x: OffsetTime, y: OffsetTime): Int = x.compareTo(y)
  }

  object PeriodValidations
      extends BaseValidationOp[Period]
      with OrderingValidation[Period]
      with ZeroValidations[Period] {

    /** What is the zero Value of N.  Used for positive and negative comparisons */
    override val zero: Period = Period.ZERO

    override def compare(x: Period, y: Period): Int = {
      if (x.getYears != y.getYears) x.getYears.compareTo(y.getYears)
      else if (x.getMonths != y.getMonths) x.getMonths.compareTo(y.getMonths)
      else x.getDays.compareTo(y.getDays)
    }
  }

  object YearValidations
      extends BaseValidationOp[Year]
      with OrderingValidation[Year]
      with ZeroValidations[Year] {

    /** What is the zero Value of N.  Used for positive and negative comparisons */
    override val zero: Year = Year.of(0)

    override def compare(x: Year, y: Year): Int = x.compareTo(y)
  }

  object YearMonthValidations
      extends BaseValidationOp[YearMonth]
      with OrderingValidation[YearMonth] {
    override def compare(x: YearMonth, y: YearMonth): Int = x.compareTo(y)
  }

  object ZoneIdValidations extends BaseValidationOp[ZoneId]

  object ZoneOffsetValidations
      extends BaseDateValidation[ZoneOffset]
      with ZeroValidations[ZoneOffset] {
    override def compare(x: ZoneOffset, y: ZoneOffset): Int = x.compareTo(y)

    /** What is the zero Value of N.  Used for positive and negative comparisons */
    override val zero: ZoneOffset = ZoneOffset.UTC

    override def defaultFormatToString(f: ZoneOffset): String = f.toString

    /** Used in the error string to describe the type.  For instance: 'date' */
    override val instantDescription: String = "zone offset"
  }

  object ZonedDateTimeValidations extends BaseDateValidation[ZonedDateTime] {
    override def compare(x: ZonedDateTime, y: ZonedDateTime): Int = x.compareTo(y)

    override def defaultFormatToString(f: ZonedDateTime): String =
      DateTimeFormatter.ISO_ZONED_DATE_TIME.format(f)

    /** Used in the error string to describe the type.  For instance: 'date' */
    override val instantDescription: String = "zoned date time"
  }

}
