package com.bones.cask.form.values

import com.bones.cask.form.{FormElement, FormElementEncoder}
import com.bones.data.values.{BaseJavaTimeInterpreter, DayOfWeekData, JavaTimeValue}
import scalatags.Text
import scalatags.Text.all._

import java.time.DateTimeException

object JavaTimeFormElement extends FormElement[JavaTimeValue] {
  override def generateFormElement[A](
    kvp: JavaTimeValue[A],
    path: List[String]): FormElementEncoder[A] = {
    val func: A => Text.TypedTag[String] = kvp match {
      case x =>
        (a: A) =>
          div(value := s"TODO: ${a.toString}")
//      case dd: DateTimeExceptionData =>
//        (s: DateTimeException) =>
//          div(`class` := "bones_date-time-exception", size := "200", value := s.getMessage)

//      case dd: DayOfWeekData =>
//        (d: DayOfWeekData)
//
//      case dd: DurationData       => durationData(dd)
//      case id: InstantData        => instantData(id)
//      case ld: LocalDateTimeData  => localDateTimeData(ld)
//      case ld: LocalDateData      => localDateData(ld)
//      case lt: LocalTimeData      => localTimeData(lt)
//      case md: MonthData          => monthData(md)
//      case md: MonthDayData       => monthDayData(md)
//      case od: OffsetDateTimeData => offsetDateTimeData(od)
//      case od: OffsetTimeData     => offsetTimeData(od)
//      case pd: PeriodData         => periodData(pd)
//      case yd: YearData           => yearData(yd)
//      case yd: YearMonthData      => yearMonthData(yd)
//      case zd: ZonedDateTimeData  => zonedDateTimeData(zd)
//      case zd: ZoneIdData         => zoneIdData(zd)
//      case zd: ZoneOffsetData     => zoneOffsetData(zd)
    }

    (a: A) =>
      withLabel(path, func(a))
  }
}
