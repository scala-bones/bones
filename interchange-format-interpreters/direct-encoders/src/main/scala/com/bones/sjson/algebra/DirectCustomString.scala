package com.bones.sjson.algebra

import com.bones.data.custom.CustomStringValue
import com.bones.sjson.JsonStringEncoderInterpreter.CustomToJsonStringInterpreter

object DirectCustomString extends CustomToJsonStringInterpreter[CustomStringValue] {
  override def toJsonString[A](alg: CustomStringValue[A]): A => List[String] = ???
}
