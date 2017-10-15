package com.gaia.soy

object IntValidation {

  trait IntValidation extends ValidationOp[Int] {
    val isValid: Int => Boolean
  }

}
