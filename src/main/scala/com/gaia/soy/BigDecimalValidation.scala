package com.gaia.soy

object BigDecimalValidation {

  case class Max(bigDecimal: BigDecimal) extends ValidationOp[BigDecimal] {
    override def isValid: (BigDecimal) => Boolean = inputBd =>  bigDecimal <= inputBd
  }
  case class Min(bigDecimal: BigDecimal) extends ValidationOp[BigDecimal] {
    override def isValid: (BigDecimal) => Boolean = inputBd => bigDecimal >= inputBd
  }

}
