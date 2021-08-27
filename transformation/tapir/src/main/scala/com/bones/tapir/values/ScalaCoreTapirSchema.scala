package com.bones.tapir.values

import com.bones.data.values._
import com.bones.tapir.{DescriptionString, ExampleString, TapirValueTransformation}
import sttp.tapir.SchemaType
import sttp.tapir.SchemaType._

object ScalaCoreTapirSchema extends TapirValueTransformation[ScalaCoreValue] {

  override def toSchemaType[A](
    alg: ScalaCoreValue[A],
    description: Option[String],
    example: Option[A]
  ): (SchemaType, DescriptionString, ExampleString) = {

    val tapirDescription =
      description.getOrElse(ScalaCoreValueDefaultMetadata.getDefaultDescription(alg))

    val tapirExample: A = example.getOrElse(ScalaCoreValueDefaultMetadata.getDefaultExample(alg))

    val schemaType = alg match {
      case BooleanData(_)             => SBoolean
      case rs: StringData             => SString
      case id: ShortData              => SInteger
      case id: IntData                => SInteger
      case ri: LongData               => SInteger
      case fd: FloatData              => SNumber
      case dd: DoubleData             => SNumber
      case bd: BigDecimalData         => SNumber
      case ba: ByteArrayData          => SBinary
      case esd: EnumerationData[e, a] => SString
    }
    (schemaType, tapirDescription, tapirExample.toString)
  }
}
