package com.bones.tapir

import java.time.format.DateTimeFormatter

import com.bones.data.values.{
  CNilF,
  CustomStringValue,
  DefaultValues,
  JavaTimeValue,
  JavaTimeValueDefaultJsonMetadata,
  JavaUtilValue,
  ScalaCoreValue
}
import com.bones.tapir.TapirValueTransformation.CNilInterchangeFormatEncoder
import shapeless.:+:

package object values {

  val defaultJavaTimeTapirSchema = new JavaTimeTapirSchema {
    override val instantFormatter: DateTimeFormatter = DateTimeFormatter.ISO_INSTANT
    override val offsetDateTimeFormatter: DateTimeFormatter =
      DateTimeFormatter.ISO_OFFSET_DATE_TIME
    override val offsetTimeFormatter: DateTimeFormatter = DateTimeFormatter.ISO_OFFSET_TIME
    override val zonedDateTimeFormatter: DateTimeFormatter =
      DateTimeFormatter.ISO_ZONED_DATE_TIME
    override val localDateFormatter: DateTimeFormatter = DateTimeFormatter.ISO_LOCAL_DATE
    override val localDateTimeFormatter: DateTimeFormatter = DateTimeFormatter.ISO_DATE_TIME
    override val localTimeFormatter: DateTimeFormatter = DateTimeFormatter.ISO_LOCAL_TIME
  }

  //  def defaultEncoders[A]: TapirEncoder[DefaultValues] =
  //    ScalaCoreTapirSchema ++
  //      (CustomStringTapirSchema ++
  //        (JavaTimeTapirSchema ++
  //          (JavaUtilTapirSchema ++ CNilInterchangeFormatEncoder)))

  // Below is equivalent to the above.  Above compiles in 2.13, below compiles in both 2.12 and 2.13
  // start 2.12

  type JavaUtilValueCo[A] = JavaUtilValue[A] :+: CNilF[A]
  type JavaTimeValueCo[A] = JavaTimeValue[A] :+: JavaUtilValueCo[A]
  type CustomStringValueCo[A] = CustomStringValue[A] :+: JavaTimeValueCo[A]

  val defaultEncoders: TapirValueTransformation[DefaultValues] = {
    TapirValueTransformation.merge[ScalaCoreValue, CustomStringValueCo](
      ScalaCoreTapirSchema,
      TapirValueTransformation.merge[CustomStringValue, JavaTimeValueCo](
        CustomStringTapirSchema,
        TapirValueTransformation.merge[JavaTimeValue, JavaUtilValueCo](
          defaultJavaTimeTapirSchema,
          TapirValueTransformation
            .merge[JavaUtilValue, CNilF](JavaUtilTapirSchema, CNilInterchangeFormatEncoder)
        )
      )
    )
  }
  // end 2.12

  val defaultTransformation = new BonesToTapirTransformation[DefaultValues] {
    override val encoder: TapirValueTransformation[DefaultValues] = defaultEncoders
  }

}
