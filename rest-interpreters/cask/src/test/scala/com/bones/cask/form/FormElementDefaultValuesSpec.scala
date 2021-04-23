package com.bones.cask.form

import com.bones.cask.form.values.defaultFormElement
import com.bones.schemas.Schemas.{AllSupported, allSupportedSchema}
import org.scalatest.funspec.AnyFunSpec

class FormElementDefaultValuesSpec extends AnyFunSpec {

  defaultFormElement.generateFormElement(allSupportedSchema, List.empty)

}
