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
    (name, index) => {
      alg match {
        case dt: DateTimeExceptionData => stringMessageField(name, index)
        case dt: DayOfWeekData         => intMessageField(name, index)
        case dd: DurationData          => stringMessageField(name, index)
        case id: InstantData           => timestampMessageField(name, index)
        case md: MonthData             => intMessageField(name, index)
        case md: MonthDayData          => intMessageField(name, index)
        case dt: OffsetDateTimeData =>
          ProtoFileGeneratorInterpreter.valueDefinition(offsetDateTimeSchema, this)(name, index)
        case dt: OffsetTimeData =>
          ProtoFileGeneratorInterpreter.valueDefinition(offsetTimeSchema, this)(name, index)
        case pd: PeriodData    => stringMessageField(name, index)
        case yd: YearData      => intMessageField(name, index)
        case ym: YearMonthData => longMessageField(name, index)
        case zd: ZonedDateTimeData =>
          ProtoFileGeneratorInterpreter.valueDefinition(zonedDateTimeSchema, this)(name, index)
        case zi: ZoneIdData     => stringMessageField(name, index)
        case zo: ZoneOffsetData => intMessageField(name, index)
      }
    }
}
