package com.bones.cask.form.values

import com.bones.Util
import com.bones.cask.form.{FormElement, FormElementEncoder}
import com.bones.data.KvpCollection
import com.bones.data.values.{
  BigDecimalData,
  BooleanData,
  ByteArrayData,
  DoubleData,
  EnumerationData,
  FloatData,
  IntData,
  LongData,
  ScalaCoreValue,
  ShortData,
  StringData
}
import scalatags.Text
import scalatags.Text.all._

object ScalaCoreFormElement extends FormElement[ScalaCoreValue] {
  override def generateFormElement[A](
    kvp: ScalaCoreValue[A],
    path: List[String]): FormElementEncoder[A] = {
    val func: A => Text.TypedTag[String] = kvp match {
      case BooleanData(_) =>
        (b: Boolean) =>
          input(`type` := "checkbox", `class` := "bones_boolean", if (b) checked else ())
      case IntData(_) =>
        (i: Int) =>
          input(`type` := "number", `class` := "bones_int", value := i)
      case LongData(_) =>
        (l: Long) =>
          input(`type` := "number", `class` := "bones_long", value := l)
      case ShortData(_) =>
        (s: Short) =>
          input(`type` := "number", `class` := "bones_short", value := s)
      case StringData(_) =>
        (str: String) =>
          input(`type` := "text", `class` := "bones_string", value := str)
      case FloatData(_) =>
        (f: Float) =>
          input(`type` := "number", `class` := "bones_float", value := f)
      case DoubleData(_) =>
        (d: Double) =>
          input(`type` := "number", `class` := "bones_double", value := d)
      case BigDecimalData(_) =>
        (bd: BigDecimal) =>
          input(`type` := "number", `class` := "bones_big-decimal", value := bd.toString)
      case ByteArrayData(_) =>
        (ba: Array[Byte]) =>
          textarea(`class` := "bones_byte-array", value := ba.map("%02X" format _).mkString)
      case EnumerationData(enumeration, _) =>
        (a: A) =>
          select(
            `class` := "bones_enumeration",
            for (v <- enumeration.values.map(_.toString).toList.sorted)
              yield option(value := v, if (a.toString == v) selected := "selected" else ())
          )
    }

    (a: A) =>
      withLabel(path, func(a))

  }
}
