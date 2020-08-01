package com.bones.protobuf.values

import java.time._

import com.bones.data.values._
import com.bones.protobuf.{ProtobufSequentialEncoderInterpreter, ProtobufEncoderValue}

trait JavaTimeEncoder extends ProtobufEncoderValue[JavaTimeValue] {

  import JavaTimeValidator._
  import ProtobufSequentialEncoderInterpreter._

  val zoneOffset: ZoneOffset

  val coreProtobufSequentialOutputInterpreter: ProtobufSequentialEncoderInterpreter

  override def encodeToProto[A](alg: JavaTimeValue[A]): EncodeToProto[A] = {
    alg match {
      case dt: DateTimeExceptionData => stringDataFromMap[DateTimeException](_.getMessage)
      case dt: DayOfWeekData         => intDataFromMap[DayOfWeek](_.getValue)
      case dd: DurationData          => stringDataFromMap[Duration](_.toString)
      case id: InstantData           => timestampFromMap(instantToTimestamp)
      case dd: LocalDateTimeData => timestampFromMap(localDateTimeToSecondsNanos(zoneOffset))
      case dt: LocalDateData     => longDataFromMap[LocalDate](_.toEpochDay)
      case lt: LocalTimeData     => longDataFromMap[LocalTime](_.toNanoOfDay)
      case md: MonthData         => intDataFromMap[Month](_.getValue)
      case md: MonthDayData      => intDataFromMap[MonthDay](monthDayToInt)
      case dt: OffsetDateTimeData => ??? //TODO
//        coreProtobufSequentialOutputInterpreter.valueDefinition(offsetDateTimeSchema, this)
      case dt: OffsetTimeData => ??? // TODO
//        coreProtobufSequentialOutputInterpreter.valueDefinition(offsetTimeSchema, this)
      case pd: PeriodData    => stringDataFromMap[Period](_.toString)
      case yd: YearData      => intDataFromMap[Year](_.getValue)
      case ym: YearMonthData => longDataFromMap(yearMonthToLong)
      case zd: ZonedDateTimeData => ??? // TODO
//        coreProtobufSequentialOutputInterpreter.valueDefinition(zonedDateTimeSchema, this)
      case zi: ZoneIdData     => stringDataFromMap(_.toString)
      case zo: ZoneOffsetData => intDataFromMap[ZoneOffset](_.getTotalSeconds)
    }
  }
}
