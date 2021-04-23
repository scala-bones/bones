package com.bones.cask.form.values

import com.bones.cask.form.{FormElement, FormElementEncoder}
import com.bones.data.values.{JavaUtilValue, UuidData}
import scalatags.Text
import scalatags.Text.all._

import java.util.UUID

object JavaUtilFormElement extends FormElement[JavaUtilValue] {
  override def generateFormElement[A](
    kvp: JavaUtilValue[A],
    path: List[String]): FormElementEncoder[A] = {
    val func: A => Text.TypedTag[String] = kvp match {
      case _: UuidData =>
        (uuid: UUID) =>
          input(`type` := "text", `class` := "bones_uuid", size := "16", value := uuid.toString)
    }

    (a: A) =>
      withLabel(path, func(a))

  }
}
