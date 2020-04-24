package com.bones.scalacheck.custom

import java.time._

import com.bones.data.custom._
import com.bones.scalacheck.{GenAlg, Scalacheck}
import com.bones.validation.ValidationDefinition.{ValidValue, ValidationOp}
import com.bones.validation.custom.JavaTimeValidation._
import org.scalacheck.Gen
import org.scalacheck.Gen.Choose

import scala.collection.JavaConverters

trait ScalacheckJavaTimeInterpreter extends GenAlg[JavaTimeValue] {

  implicit val chooseInstant = new Choose[Instant] {

    override def choose(min: Instant, max: Instant): Gen[Instant] =
      for {
        millis <- Gen.choose(min.toEpochMilli, max.toEpochMilli)
        nanos <- {
          val minNano = if (millis == min.toEpochMilli) min.getNano else 0
          val maxNano = if (millis == max.toEpochMilli) max.getNano else 999999999l
          Gen.choose(minNano, maxNano)
        }
      } yield Instant.ofEpochSecond(millis, nanos)
  }

  val chooseMonthDay = new Choose[MonthDay] {
    override def choose(min: MonthDay, max: MonthDay): Gen[MonthDay] =
      for {
        month <- Gen.choose(min.getMonthValue, max.getMonthValue).map(Month.of(_))
        day <- {
          val minDay = if (month == min.getMonthValue) min.getDayOfMonth else 1
          val maxDay = if (month == max.getMonthValue) max.getDayOfMonth else 31
          val maxGivenMonth = if (maxDay > month.maxLength) month.maxLength else maxDay
          Gen.choose(minDay, maxGivenMonth)
        }
      } yield MonthDay.of(month, day)
  }

  val chooseZoneOffset = new Choose[ZoneOffset] {
    override def choose(min: ZoneOffset, max: ZoneOffset): Gen[ZoneOffset] =
      Gen.choose(min.getTotalSeconds, max.getTotalSeconds).map(ZoneOffset.ofTotalSeconds)
  }

  val chooseZonedDateTime = new Choose[ZonedDateTime] {
    override def choose(min: ZonedDateTime, max: ZonedDateTime): Gen[ZonedDateTime] =
      for {
        localDate <- Scalacheck.chooseLocalDateTime.choose(min.toLocalDateTime, max.toLocalDateTime)
        zoneId <- genZoneId(List.empty)
      } yield ZonedDateTime.of(localDate, zoneId)

  }

  val chooseOffsetDateTime = new Choose[OffsetDateTime] {
    override def choose(min: OffsetDateTime, max: OffsetDateTime): Gen[OffsetDateTime] =
      (
        for {
          dateTime <- Scalacheck.chooseLocalDateTime.choose(
            min.toLocalDateTime,
            max.toLocalDateTime)
          offset <- chooseZoneOffset.choose(ZoneOffset.MIN, ZoneOffset.MAX)
        } yield OffsetDateTime.of(dateTime, offset)
      ).retryUntil(x => (x == min || x.isAfter(min)) && (x == max || x.isBefore(max))) // TODO: think this through, might be able to remove the 'retryUntil'.
  }

  val chooseOffsetTime = new Choose[OffsetTime] {
    override def choose(min: OffsetTime, max: OffsetTime): Gen[OffsetTime] =
      (
        for {
          time <- Scalacheck.chooseLocalTime.choose(min.toLocalTime, max.toLocalTime)
          offset <- chooseZoneOffset.choose(ZoneOffset.MIN, ZoneOffset.MAX)
        } yield OffsetTime.of(time, offset)
      ).retryUntil(x => (x == min || x.isAfter(min)) && (x == max || x.isBefore(max)))
  }

  val chooseYear = new Choose[Year] {
    override def choose(min: Year, max: Year): Gen[Year] =
      Gen.choose(Year.MIN_VALUE, Year.MAX_VALUE).map(Year.of)
  }

  val chooseDuration = new Choose[Duration] {
    override def choose(min: Duration, max: Duration): Gen[Duration] =
      for {
        seconds <- Gen.choose(Long.MinValue, Long.MaxValue)
        nanos <- Gen.choose(0, 999999999)
      } yield Duration.ofSeconds(seconds, nanos)
  }

  val choosePeriod = new Choose[Period] {
    override def choose(min: Period, max: Period): Gen[Period] =
      for {
        years <- Gen.choose(min.getYears, max.getYears)
        month <- {
          val minYears =
            if (years == min.getYears) min.getMonths else 1
          val maxYears =
            if (years == max.getYears) max.getMonths else 12
          Gen.choose(minYears, maxYears)
        }
        days <- {
          val minDays =
            if (years == min.getYears && month == min.getMonths) min.getDays else 1
          val maxDays =
            if (years == max.getYears && month == max.getMonths) max.getDays else 31
          Gen.choose(minDays, maxDays)
        }
      } yield Period.of(years, month, days)
  }

  def genYear(validations: List[ValidationOp[Year]]): Gen[Year] = {
    Scalacheck.validationConstraints[Year](
      validations,
      YearValidations,
      _.plusYears(1),
      _.minusYears(1),
      Year.of(Year.MIN_VALUE),
      Year.of(Year.MAX_VALUE)
    )(chooseYear)
  }

  def genZoneId(validations: List[ValidationOp[ZoneId]]): Gen[ZoneId] = {
    val values = validations.collectFirst {
      case v: ValidValue[_] => v.validValues.map(_.toString)
    } getOrElse {
      JavaConverters.collectionAsScalaIterable(ZoneId.getAvailableZoneIds)
    }
    Gen.oneOf(values).map(ZoneId.of)
  }

  def genZoneDateTime(validations: List[ValidationOp[ZonedDateTime]]): Gen[ZonedDateTime] = {
    val min = validations.collectFirst {
      case m: ZonedDateTimeValidations.MinTime => m.minDate.toLocalDateTime
    } getOrElse {
      LocalDateTime.MIN
    }
    val max = validations.collectFirst {
      case m: ZonedDateTimeValidations.MaxTime => m.maxDate.toLocalDateTime
    } getOrElse {
      LocalDateTime.MAX
    }

    for {
      localDateTime <- Scalacheck.chooseLocalDateTime.choose(min, max)
      zoneId <- genZoneId(List.empty)
    } yield {
      ZonedDateTime.of(localDateTime, zoneId)
    }

  }

  def genZoneOffset(validations: List[ValidationOp[ZoneOffset]]): Gen[ZoneOffset] = {
    validations.collectFirst {
      case v: ValidValue[_] => Gen.oneOf(v.validValues)
    } getOrElse {
      val min = validations.collectFirst {
        case m: ZoneOffsetValidations.MaxTime => m.maxDate
      } getOrElse {
        ZoneOffset.MIN
      }
      val max = validations.collectFirst {
        case m: ZoneOffsetValidations.MaxTime => m.maxDate
      } getOrElse {
        ZoneOffset.MAX
      }
      Gen.choose(min.getTotalSeconds, max.getTotalSeconds).map(ZoneOffset.ofTotalSeconds)
    }
  }

  override def gen[A](alg: JavaTimeValue[A]): Gen[A] =
    alg match {
      case dte: DateTimeExceptionData =>
        Scalacheck.wordsGen.map(word => new DateTimeException(word))
      case dow: DayOfWeekData =>
        val values = dow.validations
          .collectFirst[Seq[DayOfWeek]]({
            case v: ValidValue[A] => v.validValues
          })
          .getOrElse(DayOfWeek.values.toSeq)
        Gen.oneOf(values)
      case dd: DurationData =>
        Scalacheck.validationConstraints[Duration](
          dd.validations,
          DurationValidation,
          (d: Duration) => d.plusNanos(1),
          (d: Duration) => d.minusNanos(1),
          Duration.ofSeconds(Long.MaxValue, 999999999l),
          Duration.ofSeconds(Long.MinValue, 999999999l)
        )(chooseDuration)

      case id: InstantData =>
        Scalacheck.genTime(
          id.validations,
          InstantValidation,
          Instant.MIN,
          Instant.MAX,
          chooseInstant)
      case md: MonthData =>
        val values = md.validations
          .collectFirst[Seq[Month]] {
            case v: ValidValue[A] => v.validValues
          }
          .getOrElse(Month.values.toSeq)
        Gen.oneOf(values)
      case md: MonthDayData =>
        Scalacheck.genTime(
          md.validations,
          MonthDayValidations,
          MonthDay.of(1, 1),
          MonthDay.of(12, 31),
          chooseMonthDay)
      case od: OffsetDateTimeData =>
        Scalacheck.genTime(
          od.validations,
          OffsetDateTimeValidations,
          OffsetDateTime.MIN,
          OffsetDateTime.MAX,
          chooseOffsetDateTime)
      case ot: OffsetTimeData =>
        Scalacheck.genTime(
          ot.validations,
          OffsetTimeValidations,
          OffsetTime.MIN,
          OffsetTime.MAX,
          chooseOffsetTime)
      case pd: PeriodData =>
        pd.validations
          .collectFirst {
            case v: ValidValue[A] => Gen.oneOf(v.validValues)
          }
          .getOrElse {
            Scalacheck.validationConstraints[Period](
              pd.validations,
              PeriodValidations,
              _.plusDays(1),
              _.minusDays(1),
              Period.of(Int.MinValue, Int.MinValue, Int.MinValue),
              Period.of(Int.MaxValue, Int.MaxValue, Int.MaxValue)
            )(choosePeriod)
          }
      case yd: YearData => genYear(yd.validations)
      case ym: YearMonthData => {
        val minYearMonth = ym.validations
          .collectFirst {
            case min: YearMonthValidations.Min => min.minLong
          }
          .getOrElse(YearMonth.of(Year.MIN_VALUE, 1))
        val maxyearMonth = ym.validations
          .collectFirst {
            case max: YearMonthValidations.Max => max.maxLong
          }
          .getOrElse(YearMonth.of(Year.MAX_VALUE, 12))

        for {
          year <- Gen.choose(minYearMonth.getYear, maxyearMonth.getYear)
          month <- {
            val minMonth = if (year == minYearMonth.getYear) minYearMonth.getMonth.getValue else 1
            val maxMonth = if (year == maxyearMonth.getYear) maxyearMonth.getMonth.getValue else 12
            Gen.choose(minMonth, maxMonth)
          }
        } yield YearMonth.of(year, month)
      }
      case z: ZoneIdData =>
        genZoneId(z.validations)
      case z: ZonedDateTimeData =>
        Scalacheck.genTime(
          z.validations,
          ZonedDateTimeValidations,
          ZonedDateTime.of(LocalDateTime.MIN, ZoneId.of("+14:00")),
          ZonedDateTime.of(LocalDateTime.MAX, ZoneId.of("-12:00")),
          chooseZonedDateTime
        )
      case z: ZoneOffsetData =>
        Scalacheck.genTime(
          z.validations,
          ZoneOffsetValidations,
          ZoneOffset.MIN,
          ZoneOffset.MAX,
          chooseZoneOffset
        )

    }
}
