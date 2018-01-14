package com.ot.bones.validation

import com.ot.bones.interpreter.ExtractionInterpreter.ValidationOp

object BigDecimalValidation {

  case class Max(bigDecimal: BigDecimal) extends ValidationOp[BigDecimal] {
    override def isValid: (BigDecimal) => Boolean = inputBd =>  bigDecimal <= inputBd

    override def defaultError(t: BigDecimal): String = ???

    override def description: String = ???
  }
  case class Min(bigDecimal: BigDecimal) extends ValidationOp[BigDecimal] {
    override def isValid: (BigDecimal) => Boolean = inputBd => bigDecimal >= inputBd

    override def defaultError(t: BigDecimal): String = ???

    override def description: String = ???
  }

}
