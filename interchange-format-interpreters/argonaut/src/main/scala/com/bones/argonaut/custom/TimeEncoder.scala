package com.bones.argonaut.custom

import java.time.Instant
import java.time.format.DateTimeFormatter

import argonaut.Json
import com.bones.data.custom._
import com.bones.interpreter.KvpInterchangeFormatEncoderInterpreter.InterchangeFormatEncoder

trait IsoTimeEncoder extends TimeEncoder {
  override val instantFormatter: DateTimeFormatter = DateTimeFormatter.ISO_INSTANT
}

trait TimeEncoder extends InterchangeFormatEncoder[JavaTimeValue, Json] {

  val instantFormatter: DateTimeFormatter

  override def encode[A](alg: JavaTimeValue[A]): A => Json =
    alg match {
      case DurationData(_) => duration => Json.jString(duration.toString)
      case InstantData(_) => instant => Json.jString(instantFormatter.format(instant))
      case MonthData(_) => month => Json.jString(month.toString)
      case PeriodData(_) => period => Json.jString(period.toString)
      case ZoneIdData(_) => zoneId => Json.jString(zoneId.toString)
      case ZoneOffsetData(_) => zoneOffset => Json.jString(zoneOffset.toString)
    }
}
