package com.bones.swagger
import java.time.format.DateTimeFormatter

import com.bones.data.values.DefaultValues
import com.bones.swagger.SwaggerCoreInterpreter.CustomSwaggerInterpreter
import com.bones.swagger.SwaggerCoreInterpreter.CustomSwaggerInterpreter.CNilCustomSwaggerInterpreter

package object values {

  val defaultInterpreters: CustomSwaggerInterpreter[DefaultValues] =
    DefaultScalaCoreInterpreter ++
      (DefaultCustomStringInterpreter ++
        (SwaggerIsoJavaTimeInterpreter ++
          (DefaultJavaUtilInterpreter ++ CNilCustomSwaggerInterpreter)))

  object DefaultCustomStringInterpreter extends CustomStringInterpreter
  object DefaultJavaUtilInterpreter extends JavaUtilInterpreter
  object DefaultScalaCoreInterpreter extends ScalaCoreInterpreter

  object SwaggerIsoJavaTimeInterpreter extends JavaTimeInterpreter {
    override val instantFormatter: DateTimeFormatter = DateTimeFormatter.ISO_INSTANT
    override val offsetDateTimeFormatter: DateTimeFormatter = DateTimeFormatter.ISO_OFFSET_DATE_TIME
    override val offsetTimeFormatter: DateTimeFormatter = DateTimeFormatter.ISO_OFFSET_TIME
    override val zonedDateTimeFormatter: DateTimeFormatter = DateTimeFormatter.ISO_ZONED_DATE_TIME
    override val localDateTimeFormatter: DateTimeFormatter = DateTimeFormatter.ISO_DATE_TIME
    override val localDateFormatter: DateTimeFormatter = DateTimeFormatter.ISO_LOCAL_DATE
    override val localTimeFormatter: DateTimeFormatter = DateTimeFormatter.ISO_TIME
  }

}
