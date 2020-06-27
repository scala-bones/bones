package com.bones.sjson.values

import com.bones.data.values.CustomStringValue
import com.bones.sjson.JsonStringEncoderInterpreter.CustomToJsonStringInterpreter

object DirectCustomString extends CustomToJsonStringInterpreter[CustomStringValue] {
  override def toJsonString[A](alg: CustomStringValue[A]): A => List[String] = ???
}
