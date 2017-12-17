package com.gaia.soy.compiler

import com.gaia.soy.Soyo.ObjectFieldGroup
import com.gaia.soy.StringValidation.{OptionalString, RequiredString}
import com.gaia.soy.{FieldGroupOp, JsonProducer}

object JsonCompiler {
  // a function that takes a JsonProducer as input
  type FromProducer[A] = JsonProducer => A
//  type FromProducer[A] = JsonProducer => Validated[NonEmptyList[ExtractionError], A]

  val defaultCompiler = new cats.arrow.FunctionK[FieldGroupOp, FromProducer] {
    def apply[A](fa: FieldGroupOp[A]): FromProducer[A] = strPro =>
      fa match {
        case rs: RequiredString => rs.extract(strPro)
        case os: OptionalString  => os.extract(strPro)
        case os: ObjectFieldGroup[_,_] => os.extract(strPro).asInstanceOf[A]

        case _ => ???
      }
  }
}
