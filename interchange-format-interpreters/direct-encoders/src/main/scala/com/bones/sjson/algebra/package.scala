package com.bones.sjson

import com.bones.data.custom.AllCustomAlgebras
import com.bones.sjson.JsonStringEncoderInterpreter.CustomToJsonStringInterpreter
import com.bones.sjson.JsonStringEncoderInterpreter.CustomToJsonStringInterpreter.CNilCustomEncoder

package object algebra {

  def allEncoders[A]: CustomToJsonStringInterpreter[AllCustomAlgebras] =
    DirectScalaCore ++ (DirectCustomString ++ (DirectIsoJavaTime ++ (DirectJavaUtil ++ CNilCustomEncoder)))

}
