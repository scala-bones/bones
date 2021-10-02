package com.bones.tapir.values

import com.bones.data.values.CustomStringValue
import com.bones.tapir.{DescriptionString, ExampleString, TapirValueTransformation}
import sttp.tapir.SchemaType
import sttp.tapir.SchemaType.SString

object CustomStringTapirSchema extends TapirValueTransformation[CustomStringValue] {

  override def toSchemaType[A](
    alg: CustomStringValue[A],
    description: Option[String],
    example: Option[A]
  ): (SchemaType, DescriptionString, ExampleString) = {
    (
      SString,
      description.getOrElse(alg.description),
      example.map(_.asInstanceOf[String]).getOrElse(alg.example)
    )
  }

}
