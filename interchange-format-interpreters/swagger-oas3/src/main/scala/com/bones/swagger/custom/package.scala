package com.bones.swagger
import java.time.format.DateTimeFormatter

import com.bones.data.custom.{AllCustomAlgebras, CustomStringCoproduct}
import com.bones.swagger.SwaggerCoreInterpreter.CustomSwaggerInterpreter
import com.bones.swagger.SwaggerCoreInterpreter.CustomSwaggerInterpreter.CNilCustomSwaggerInterpreter

package object custom {

  val allInterpreters: CustomSwaggerInterpreter[AllCustomAlgebras] =
    SwaggerIsoJavaTimeInterpreter ++
      (DefaultCustomStringInterpreter ++ CNilCustomSwaggerInterpreter: CustomSwaggerInterpreter[CustomStringCoproduct])

  object DefaultCustomStringInterpreter extends CustomStringInterpreter

  object SwaggerIsoJavaTimeInterpreter extends JavaTimeInterpreter {
    override val instantFormatter: DateTimeFormatter = DateTimeFormatter.ISO_INSTANT
    override val offsetDateTimeFormatter: DateTimeFormatter = DateTimeFormatter.ISO_OFFSET_DATE_TIME
    override val offsetTimeFormatter: DateTimeFormatter = DateTimeFormatter.ISO_OFFSET_TIME
    override val zonedDateTimeFormatter: DateTimeFormatter = DateTimeFormatter.ISO_ZONED_DATE_TIME
  }

}
