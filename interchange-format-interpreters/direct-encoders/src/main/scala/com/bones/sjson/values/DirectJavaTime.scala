package com.bones.sjson.values

import java.time.format.DateTimeFormatter

import com.bones.data.values.{JavaTimeValue, LocalDateData, LocalDateTimeData, LocalTimeData}
import com.bones.sjson.JsonStringEncoderInterpreter.CustomToJsonStringInterpreter
import org.apache.commons.text.StringEscapeUtils.escapeJson

object DirectIsoJavaTime extends CustomToJsonStringInterpreter[JavaTimeValue] {

  val localDateTimeFormatter: DateTimeFormatter = DateTimeFormatter.ISO_LOCAL_DATE_TIME
  val localDateFormatter: DateTimeFormatter = DateTimeFormatter.ISO_LOCAL_DATE
  val localTimeFormatter: DateTimeFormatter = DateTimeFormatter.ISO_DATE

  override def toJsonString[A](alg: JavaTimeValue[A]): A => List[String] =
    alg match {
      case ld: LocalDateData =>
        d =>
          List("\"" + escapeJson(localDateFormatter.format(d)) + "\"")
      case dd: LocalDateTimeData =>
        d =>
          List("\"" + escapeJson(localDateTimeFormatter.format(d)) + "\"")
      case lt: LocalTimeData =>
        d =>
          List("\"" + escapeJson(localTimeFormatter.format(d)) + "\"")
      case _ => sys.error("This ain't ready for prime time yet.")
    }
}
