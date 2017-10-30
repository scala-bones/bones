package com.gaia.soy.compiler

import com.gaia.soy.{FieldGroupOp, JsonProducer, ValidationResultNel}
import com.gaia.soy.StringValidation.{OptionalString, RequiredString}

object JsonCompiler {
  // a function that takes a JsonProducer as input
  type FromProducer[A] = JsonProducer => A

  val defaultCompiler = new cats.arrow.FunctionK[FieldGroupOp, FromProducer] {
    def apply[A](fa: FieldGroupOp[A]): FromProducer[A] = strPro =>
      fa match {
        case rs: RequiredString => rs.extract(strPro)
        case os: OptionalString  => os.extract(strPro)
      }
  }
}
