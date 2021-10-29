package com.bones.sjson.values

import java.util.Base64

import com.bones.data.values.{
  BigDecimalData,
  BooleanData,
  ByteArrayData,
  DoubleData,
  EnumerationData,
  FloatData,
  IntData,
  LocalDateData,
  LocalDateTimeData,
  LocalTimeData,
  LongData,
  ScalaCoreValue,
  ShortData,
  StringData,
  UuidData
}
import com.bones.sjson.JsonStringEncoderInterpreter.CustomToJsonStringInterpreter
import org.apache.commons.text.StringEscapeUtils.escapeJson

object DirectScalaCore extends CustomToJsonStringInterpreter[ScalaCoreValue] {
  override def toJsonString[A](alg: ScalaCoreValue[A]): A => List[String] =
    alg match {
      case ob: BooleanData =>
        b => if (b) List("true") else List("false")
      case rs: StringData =>
        s => List("\"" + escapeJson(s) + "\"")
      case ri: LongData =>
        l => List(l.toString)
      case bd: BigDecimalData =>
        bd => List(bd.toString)
      case dd: DoubleData =>
        d => List(d.toString)
      case fd: FloatData =>
        f => List(f.toString)
      case id: IntData =>
        i => List(i.toString)
      case sd: ShortData =>
        s => List(s.toString)
      case ba: ByteArrayData =>
        (input: Array[Byte]) =>
          List("\"", escapeJson(Base64.getEncoder.encodeToString(input)), "\"")
      case esd: EnumerationData[e, a] =>
        e => List("\"", escapeJson(e.toString), "\"")

    }
}
