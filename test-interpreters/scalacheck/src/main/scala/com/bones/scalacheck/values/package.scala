package com.bones.scalacheck

import com.bones.data.values.{
  CNilF,
  CustomStringValue,
  DefaultValues,
  JavaTimeValue,
  JavaUtilValue,
  ScalaCoreValue
}
import com.bones.scalacheck.GenValue.CNilGenEncoder
import shapeless.{:+:, CNil}

package object values {

//  val allInterpreters: GenValue[DefaultValues] = ???
//    DefaultScalacheckScalaCoreInterpreter ++
//      (DefaultCustomStringValueInterpreter ++
//        (DefaultScalacheckJavaTimeInterpreter ++
//          (GenValue.merge(DefaultScalacheckJavaUtilInterpreter, CNilGenEncoder))))

  // Below is equivalent to the above.  Above compiles in 2.13, below compiles in both 2.12 and 2.13
  //start 2.12

  type JavaUtilValueCo[A] = JavaUtilValue[A] :+: CNilF[A]
  type JavaTimeValueCo[A] = JavaTimeValue[A] :+: JavaUtilValueCo[A]
  type CustomStringValueCo[A] = CustomStringValue[A] :+: JavaTimeValueCo[A]

  val allInterpreters: GenValue[DefaultValues] = {
    GenValue.merge[ScalaCoreValue, CustomStringValueCo](
      DefaultScalacheckScalaCoreInterpreter,
      GenValue.merge[CustomStringValue, JavaTimeValueCo](
        DefaultCustomStringValueInterpreter,
        GenValue.merge[JavaTimeValue, JavaUtilValueCo](
          DefaultScalacheckJavaTimeInterpreter,
          GenValue
            .merge[JavaUtilValue, CNilF](DefaultScalacheckJavaUtilInterpreter, CNilGenEncoder))
      )
    )
  }
  //end 2.12

  object DefaultScalacheckJavaTimeInterpreter extends ScalacheckJavaTimeInterpreter
  object DefaultCustomStringValueInterpreter extends CustomStringValueInterpreter
  object DefaultScalacheckScalaCoreInterpreter extends ScalacheckScalaCoreInterpreter
  object DefaultScalacheckJavaUtilInterpreter extends ScalacheckJavaUtilInterpreter

  def defaultValuesScalacheck[K] = new ScalacheckBase[K, DefaultValues] {
    override val genValue: GenValue[DefaultValues] = allInterpreters
  }

}
