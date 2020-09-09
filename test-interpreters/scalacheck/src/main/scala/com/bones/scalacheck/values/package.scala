package com.bones.scalacheck

import com.bones.data.values.DefaultValues
import com.bones.scalacheck.GenValue.CNilGenEncoder

package object values {

  val allInterpreters: GenValue[DefaultValues] =
    DefaultScalacheckScalaCoreInterpreter ++
      (DefaultCustomStringValueInterpreter ++
        (DefaultScalacheckJavaTimeInterpreter ++
          (DefaultScalacheckJavaUtilInterpreter ++
            CNilGenEncoder)))

  object DefaultScalacheckJavaTimeInterpreter extends ScalacheckJavaTimeInterpreter
  object DefaultCustomStringValueInterpreter extends CustomStringValueInterpreter
  object DefaultScalacheckScalaCoreInterpreter extends ScalacheckScalaCoreInterpreter
  object DefaultScalacheckJavaUtilInterpreter extends ScalacheckJavaUtilInterpreter

  val defaultValuesScalacheck = new ScalacheckBase[DefaultValues] {
    override val genValue: GenValue[DefaultValues] = allInterpreters
  }

}
