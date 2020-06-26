package com.bones.scalacheck

import com.bones.data.custom.AllCustomAlgebras
import com.bones.scalacheck.GenAlg.CNilGenEncoder

package object custom {

  val allInterpreters: GenAlg[AllCustomAlgebras] =
    DefaultScalacheckScalaCoreInterpreter ++
      (DefaultCustomStringValueInterpreter ++
      (DefaultScalacheckJavaTimeInterpreter ++
      (DefaultScalacheckJavaUtilInterpreter ++
      CNilGenEncoder)))

  object DefaultScalacheckJavaTimeInterpreter extends ScalacheckJavaTimeInterpreter
  object DefaultCustomStringValueInterpreter extends CustomStringValueInterpreter
  object DefaultScalacheckScalaCoreInterpreter extends ScalacheckScalaCoreInterpreter
  object DefaultScalacheckJavaUtilInterpreter extends ScalacheckJavaUtilInterpreter

}
