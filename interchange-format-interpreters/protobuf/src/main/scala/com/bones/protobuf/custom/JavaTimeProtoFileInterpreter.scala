package com.bones.protobuf.custom

import com.bones.data.custom._
import com.bones.protobuf.ProtoFileGeneratorInterpreter

object JavaTimeProtoFileInterpreter
    extends ProtoFileGeneratorInterpreter.CustomInterpreter[JavaTimeValue] {

  import JavaTimeValidatorInterpreter._
  import ProtoFileGeneratorInterpreter._

  override def toMessageField[A](alg: JavaTimeValue[A]): (Name, Int) => (
    ProtoFileGeneratorInterpreter.MessageField,
    Vector[ProtoFileGeneratorInterpreter.NestedType],
    Int) =
    {
      alg match {
        case dt: DateTimeExceptionData => (name, index) => stringMessageField(name, index)
        case dt: DayOfWeekData         => (name, index) => intMessageField(name, index)
        case dd: DurationData          => (name, index) => stringMessageField(name, index)
        case id: InstantData           => (name, index) => timestampMessageField(name, index)
        case dd: LocalDateTimeData => (name, index) => timestampMessageField(name, index)
        case dt: LocalDateData     => (name, index) => longMessageField(name, index)
        case lt: LocalTimeData     => (name, index) => longMessageField(name, index)
        case md: MonthData             => (name, index) => intMessageField(name, index)
        case md: MonthDayData          => (name, index) => intMessageField(name, index)
//        case dt: OffsetDateTimeData =>
//          ProtoFileGeneratorInterpreter.valueDefinition(offsetDateTimeSchema, this)
//        case dt: OffsetTimeData =>
//          ProtoFileGeneratorInterpreter.valueDefinition(offsetTimeSchema, this)
        case pd: PeriodData    => (name, index) => stringMessageField(name, index)
        case yd: YearData      => (name, index) => intMessageField(name, index)
        case ym: YearMonthData => (name, index) => longMessageField(name, index)
//        case zd: ZonedDateTimeData =>
//          ProtoFileGeneratorInterpreter.valueDefinition(zonedDateTimeSchema, this)
        case zi: ZoneIdData     => (name, index) => stringMessageField(name, index)
        case zo: ZoneOffsetData => (name, index) => intMessageField(name, index)
      }
    }
}
