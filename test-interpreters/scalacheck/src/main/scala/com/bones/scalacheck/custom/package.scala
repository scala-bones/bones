package com.bones.scalacheck

import com.bones.data.custom.{AllCustomAlgebras, CustomStringCoproduct}
import com.bones.scalacheck.GenAlg.CNilGenEncoder

package object custom {

  val allInterpreters: GenAlg[AllCustomAlgebras] =
    DefaultScalacheckJavaTimeInterpreter ++
      (DefaultCustomStringValueInterpreter ++ CNilGenEncoder:GenAlg[CustomStringCoproduct])


  object DefaultScalacheckJavaTimeInterpreter extends ScalacheckJavaTimeInterpreter
  object DefaultCustomStringValueInterpreter extends CustomStringValueInterpreter

}
