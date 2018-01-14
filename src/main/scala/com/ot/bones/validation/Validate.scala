package com.ot.bones.validation

import com.ot.bones.ProgramModuleOp
import com.ot.bones.interpreter.ExtractionInterpreter.ValidationResultNel
import com.ot.bones.validation.Validate.ValidateData

object Validate {

  case class ValidateData[I,T](dataDefinitionOp: DataDefinitionOp[T]) extends ProgramModuleOp[I => ValidationResultNel[T]]

}



trait ValidateSyntax {

  def validate[I,T](dataDefinitionOp: DataDefinitionOp[T]): ValidateData[I,T] = ValidateData[I,T](dataDefinitionOp)

}


