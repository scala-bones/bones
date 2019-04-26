package com.bones.react

import com.bones.react.FormInterpreter.{ReactComponent, ReactComponentReference, ReactFormValue}

object ComponentToFile {

  type FileName = String
  type FileContents = String

  def convert(components: List[ReactComponent]): (FileName, FileContents) = ???

  private def constructorData(formValues: ReactFormValue, components: List[ReactComponent]) = {
    val default = formValues.inputType match {
      case ref: ReactComponentReference => s"    ${formValues.label}: {}"
      case _ =>                            s"    ${formValues.label}: ''"
    }
  }

  def constructor(components: ReactComponent): String = {
    s"""
       |  constructor(props) {
       |    this.state = {
       |
       |    }
       |
     """.stripMargin
  }


}
