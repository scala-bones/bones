package com.bones.tapir.values

import java.util.UUID

import com.bones.data.values.{JavaUtilValue, UuidData}
import com.bones.tapir.{DescriptionString, ExampleString, TapirValueTransformation}
import sttp.tapir.SchemaType
import sttp.tapir.SchemaType.SString

object JavaUtilTapirSchema extends TapirValueTransformation[JavaUtilValue] {
  val exampleUuid = UUID.fromString("322dd565-0a28-4959-9b7e-42ba84149870")

  override def toSchemaType[A](
    alg: JavaUtilValue[A],
    description: Option[String],
    example: Option[A]
  ): (SchemaType, DescriptionString, ExampleString) = {
    alg match {
      case uu: UuidData =>
        (
          SString,
          description.getOrElse("value of type UUID"),
          example.getOrElse(exampleUuid).asInstanceOf[UUID].toString
        )
    }
  }
}
