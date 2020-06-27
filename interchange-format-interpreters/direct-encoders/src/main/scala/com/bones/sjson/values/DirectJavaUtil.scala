package com.bones.sjson.values

import com.bones.data.values.{JavaUtilValue, UuidData}
import com.bones.sjson.JsonStringEncoderInterpreter.CustomToJsonStringInterpreter

object DirectJavaUtil extends CustomToJsonStringInterpreter[JavaUtilValue] {
  override def toJsonString[A](alg: JavaUtilValue[A]): A => List[String] = {
    alg match {
      case uu: UuidData =>
        u =>
          List("\"" + u.toString + "\"")
    }
  }
}
