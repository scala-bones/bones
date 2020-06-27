package com.bones.sjson

import com.bones.data.values.DefaultValues
import com.bones.sjson.JsonStringEncoderInterpreter.CustomToJsonStringInterpreter
import com.bones.sjson.JsonStringEncoderInterpreter.CustomToJsonStringInterpreter.CNilCustomEncoder

package object values {

  def allEncoders[A]: CustomToJsonStringInterpreter[DefaultValues] =
    DirectScalaCore ++ (DirectCustomString ++ (DirectIsoJavaTime ++ (DirectJavaUtil ++ CNilCustomEncoder)))

}
