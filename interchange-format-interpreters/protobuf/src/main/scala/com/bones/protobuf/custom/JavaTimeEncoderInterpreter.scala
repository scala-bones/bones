package com.bones.protobuf.custom

import java.time._

import com.bones.data.custom._
import com.bones.protobuf.ProtobufSequentialEncoderInterpreter

trait JavaTimeEncoderInterpreter extends ProtobufSequentialEncoderInterpreter.CustomInterpreter[JavaTimeValue] {

  import JavaTimeValidatorInterpreter._
  import ProtobufSequentialEncoderInterpreter._

  val coreProtobufSequentialOutputInterpreter: ProtobufSequentialEncoderInterpreter

  override def encodeToProto[A](alg: JavaTimeValue[A]): EncodeToProto[A] = {
    alg match {
      case dt: DateTimeExceptionData => stringDataFromMap[DateTimeException](_.getMessage)
      case dt: DayOfWeekData => intDataFromMap[DayOfWeek](_.getValue)
      case dd: DurationData => stringDataFromMap[Duration](_.toString)
      case id: InstantData => timestampFromMap(instantToTimestamp)
      case md: MonthData => intDataFromMap[Month](_.getValue)
      case md: MonthDayData => intDataFromMap[MonthDay](monthDayToInt)
      case dt: OffsetDateTimeData =>
        coreProtobufSequentialOutputInterpreter.valueDefinition(offsetDateTimeSchema, this)
      case dt: OffsetTimeData =>
        coreProtobufSequentialOutputInterpreter.valueDefinition(offsetTimeSchema, this)
      case pd: PeriodData => stringDataFromMap[Period](_.toString)
      case yd: YearData => intDataFromMap[Year](_.getValue)
      case ym: YearMonthData => longDataFromMap(yearMonthToLong)
      case zd: ZonedDateTimeData =>
        coreProtobufSequentialOutputInterpreter.valueDefinition(zonedDateTimeSchema, this)
      case zi: ZoneIdData => stringDataFromMap(_.toString)
      case zo: ZoneOffsetData => intDataFromMap[ZoneOffset](_.getTotalSeconds)
    }
  }
}