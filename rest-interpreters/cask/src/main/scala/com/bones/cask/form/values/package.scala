package com.bones.cask.form

import com.bones.Util
import com.bones.cask.form.FormElement.CNilFormElement
import com.bones.data.values.{
  CNilF,
  CustomStringValue,
  DefaultValues,
  JavaTimeValue,
  JavaUtilValue,
  ScalaCoreValue
}
import com.bones.interpreter.encoder.InterchangeFormatEncoderValue
import com.bones.interpreter.encoder.InterchangeFormatEncoderValue.CNilInterchangeFormatEncoder
import scalatags.Text
import scalatags.Text.all._
import shapeless.:+:

package object values {

//  val defaultFormElement: FormElement[DefaultValues] =
//    ScalaCoreFormElement ++ (CustomStringFormElement ++ (JavaTimeFormElement ++
//      (JavaUtilFormElement ++ CNilFormElement())))

  // Below is equivalent to the above.  Above compiles in 2.13, below compiles in both 2.12 and 2.13
  //start 2.12

  type JavaUtilValueCo[A] = JavaUtilValue[A] :+: CNilF[A]
  type JavaTimeValueCo[A] = JavaTimeValue[A] :+: JavaUtilValueCo[A]
  type CustomStringValueCo[A] = CustomStringValue[A] :+: JavaTimeValueCo[A]

  val defaultFormElement: FormElement[DefaultValues] = {
    FormElement.merge[ScalaCoreValue, CustomStringValueCo](
      ScalaCoreFormElement,
      FormElement.merge[CustomStringValue, JavaTimeValueCo](
        CustomStringFormElement,
        FormElement.merge[JavaTimeValue, JavaUtilValueCo](
          JavaTimeFormElement,
          FormElement
            .merge[JavaUtilValue, CNilF](JavaUtilFormElement, CNilFormElement)
        )
      )
    )
  }

  def withLabel[A](path: List[String], element: Text.TypedTag[String]): Text.TypedTag[String] = {
    val name = path.lastOption.map(Util.camelToWords)
    val identifier = path.mkString(".")

    div(
      label(`for` := identifier, name),
      element(id := path.mkString("."))
    )
  }
}
