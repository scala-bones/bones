package com.bones.protobuf.messageType

import com.bones.data.values._

object JavaTimeProtoFileInterpreter extends CustomInterpreter[JavaTimeValue] {

  import ProtoFileGeneratorInterpreter._

  override def toMessageField[A](
    alg: JavaTimeValue[A]
  ): (Name, Int) => (MessageField, Vector[NestedType], Int) = {
    alg match {
      case dt: DateTimeExceptionData =>
        (name, index) =>
          stringMessageField(name, index)
      case dt: DayOfWeekData =>
        (name, index) =>
          intMessageField(name, index)
      case dd: DurationData =>
        (name, index) =>
          stringMessageField(name, index)
      case id: InstantData =>
        (name, index) =>
          timestampMessageField(name, index)
      case dd: LocalDateTimeData =>
        (name, index) =>
          timestampMessageField(name, index)
      case dt: LocalDateData =>
        (name, index) =>
          longMessageField(name, index)
      case lt: LocalTimeData =>
        (name, index) =>
          longMessageField(name, index)
      case md: MonthData =>
        (name, index) =>
          intMessageField(name, index)
      case md: MonthDayData =>
        (name, index) =>
          intMessageField(name, index)
      case dt: OffsetDateTimeData => ??? //TODO
//          ProtoFileGeneratorInterpreter.valueDefinition(offsetDateTimeSchema, this)
      case dt: OffsetTimeData => ??? //TODO
//          ProtoFileGeneratorInterpreter.valueDefinition(offsetTimeSchema, this)
      case pd: PeriodData =>
        (name, index) =>
          stringMessageField(name, index)
      case yd: YearData =>
        (name, index) =>
          intMessageField(name, index)
      case ym: YearMonthData =>
        (name, index) =>
          longMessageField(name, index)
      case zd: ZonedDateTimeData => ??? //TODO
//          ProtoFileGeneratorInterpreter.valueDefinition(zonedDateTimeSchema, this)
      case zi: ZoneIdData =>
        (name, index) =>
          stringMessageField(name, index)
      case zo: ZoneOffsetData =>
        (name, index) =>
          intMessageField(name, index)
    }
  }
}
