package com.bones.swagger
import java.time.format.DateTimeFormatter

import com.bones.data.values.{
  CNilF,
  CustomStringValue,
  DefaultValues,
  JavaTimeValue,
  JavaUtilValue,
  ScalaCoreValue
}
import com.bones.interpreter.InterchangeFormatEncoderValue
import com.bones.swagger.SwaggerCoreInterpreter.CustomSwaggerInterpreter
import com.bones.swagger.SwaggerCoreInterpreter.CustomSwaggerInterpreter.CNilCustomSwaggerInterpreter
import shapeless.:+:

package object values {

//  val defaultInterpreters: CustomSwaggerInterpreter[DefaultValues] =
//    DefaultScalaCoreInterpreter ++
//      (DefaultCustomStringInterpreter ++
//        (SwaggerIsoJavaTimeInterpreter ++
//          (DefaultJavaUtilInterpreter ++ CNilCustomSwaggerInterpreter)))

  type JavaUtilValueCo[A] = JavaUtilValue[A] :+: CNilF[A]
  type JavaTimeValueCo[A] = JavaTimeValue[A] :+: JavaUtilValueCo[A]
  type CustomStringValueCo[A] = CustomStringValue[A] :+: JavaTimeValueCo[A]

  val defaultInterpreters: CustomSwaggerInterpreter[DefaultValues] = {
    CustomSwaggerInterpreter.merge[ScalaCoreValue, CustomStringValueCo](
      DefaultScalaCoreInterpreter,
      CustomSwaggerInterpreter.merge[CustomStringValue, JavaTimeValueCo](
        DefaultCustomStringInterpreter,
        CustomSwaggerInterpreter.merge[JavaTimeValue, JavaUtilValueCo](
          SwaggerIsoJavaTimeInterpreter,
          CustomSwaggerInterpreter
            .merge[JavaUtilValue, CNilF](DefaultJavaUtilInterpreter, CNilCustomSwaggerInterpreter)
        )
      )
    )
  }

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

  val defaultSwaggerInterpreter = new SwaggerCoreInterpreter[DefaultValues] {
    override def customInterpreter: CustomSwaggerInterpreter[DefaultValues] = defaultInterpreters
  }

}
