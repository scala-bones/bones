package com.bones.swagger.values

import com.bones.data.values._
import com.bones.swagger.SwaggerCoreInterpreter
import com.bones.swagger.SwaggerCoreInterpreter.{CustomSwaggerInterpreter, Name}
import io.swagger.v3.oas.models.media.Schema

trait ScalaCoreInterpreter extends CustomSwaggerInterpreter[ScalaCoreValue] {
  override def toSchema[A](
    scv: ScalaCoreValue[A],
    description: Option[String],
    example: Option[A]
  ): Name => SwaggerCoreInterpreter.SwaggerSchemas[Schema[_]] = {

    import SwaggerCoreInterpreter._

    val swaggerDescription =
      description.getOrElse(ScalaCoreValueDefaultMetadata.getDefaultDescription(scv))
    val swaggerExample: A = example.getOrElse(ScalaCoreValueDefaultMetadata.getDefaultExample(scv))

    scv match {
      case bd: BooleanData =>
        name => addBooleanSchema(name, description, example, validations(bd.validations))
      case sd: StringData =>
        name =>
          addStringSchema(
            name,
            swaggerDescription,
            swaggerExample.asInstanceOf[String],
            validations(sd.validations)
          )
      case sd: ShortData =>
        name => {
          addShortSchema(
            name,
            swaggerDescription,
            swaggerExample.asInstanceOf[Short],
            validations(sd.validations)
          )
        }
      case id: IntData =>
        name =>
          addIntSchema(
            name,
            swaggerDescription,
            swaggerExample.asInstanceOf[Int],
            validations(id.validations)
          )
      case ld: LongData =>
        name =>
          addLongSchema(
            name,
            swaggerDescription,
            swaggerExample.asInstanceOf[Long],
            validations(ld.validations)
          )
      case dd: DoubleData =>
        name =>
          addNumberSchema(
            name,
            swaggerDescription,
            swaggerExample.asInstanceOf[Double].toString,
            validations(dd.validations)
          )
      case fd: FloatData =>
        name =>
          addNumberSchema(
            name,
            swaggerDescription,
            swaggerExample.asInstanceOf[Float].toString,
            validations(fd.validations)
          )
      case bd: BigDecimalData =>
        name =>
          addStringSchema(
            name,
            swaggerDescription,
            swaggerExample.asInstanceOf[BigDecimal].toString,
            validations(bd.validations)
          )
      case ba: ByteArrayData =>
        name =>
          addBase64ByteArraySchema(
            name,
            swaggerDescription,
            swaggerExample.asInstanceOf[Array[Byte]],
            validations(ba.validations)
          )
      case esd: EnumerationData[e, a] @unchecked =>
        name =>
          addEnumerationData(
            name,
            swaggerDescription,
            swaggerExample.toString,
            esd.enumeration.values.toList.map(_.toString),
            validations(esd.validations)
          )

    }
  }
}
