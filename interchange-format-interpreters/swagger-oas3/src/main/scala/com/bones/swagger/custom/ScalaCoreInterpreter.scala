package com.bones.swagger.custom

import com.bones.data.custom._
import com.bones.swagger.SwaggerCoreInterpreter
import com.bones.swagger.SwaggerCoreInterpreter.{CustomSwaggerInterpreter, Name}
import io.swagger.v3.oas.models.media.Schema

trait ScalaCoreInterpreter extends CustomSwaggerInterpreter[ScalaCoreValue]{
  override def toSchema[A](scv: ScalaCoreValue[A], description: Option[String], example: Option[A]):
    Name => SwaggerCoreInterpreter.SwaggerSchemas[Schema[_]] = {

    import SwaggerCoreInterpreter._

    scv match {
      case bd: BooleanData =>
        name =>
          addBooleanSchema(name, description, example, validations(bd.validations))
      case sd: StringData =>
        name =>
          addStringSchema(
            name,
            description.getOrElse("value of type string"),
            example.getOrElse("ABC"),
            validations(sd.validations))
      case sd: ShortData =>
        name =>
        {
          addShortSchema(
            name,
            description.getOrElse("value of type short"),
            example.asInstanceOf[Option[Short]].getOrElse((123: Short)),
            validations(sd.validations))
        }
      case id: IntData =>
        name =>
          addIntSchema(
            name,
            description.getOrElse("value of type integer"),
            example.asInstanceOf[Option[Int]].getOrElse(123),
            validations(id.validations)
          )
      case ld: LongData =>
        name =>
          addLongSchema(
            name,
            description.getOrElse("value of type long"),
            example.asInstanceOf[Option[Long]].getOrElse(123l),
            validations(ld.validations)
          )
      case dd: DoubleData =>
        name =>
          addNumberSchema(
            name,
            description.getOrElse("value of type double"),
            example.map(_.toString).getOrElse("3.14"),
            validations(dd.validations)
          )
      case fd: FloatData =>
        name =>
          addNumberSchema(
            name,
            description.getOrElse("value of type float"),
            example.map(_.toString).getOrElse("3.14"),
            validations(fd.validations)
          )
      case bd: BigDecimalData =>
        name =>
          addStringSchema(
            name,
            description.getOrElse("value fo type big decimal"),
            example.getOrElse(BigDecimal("3.14")).toString,
            validations(bd.validations)
          )
      case ba: ByteArrayData =>
        name =>
          addBase64ByteArraySchema(
            name,
            description.getOrElse("base64 encoded byte array"),
            example.getOrElse("0123456789abcdef".getBytes).asInstanceOf[Array[Byte]],
            validations(ba.validations)
          )
      case esd: EnumerationData[e, a] @unchecked =>
        name =>
          addEnumerationData(
            name.toString,
            description.getOrElse(s"enumeration of type ${esd.manifestOfA.getClass.getSimpleName}"),
            example.orElse(esd.enumeration.values.headOption).map(_.toString).getOrElse(""),
            esd.enumeration.values.toList.map(_.toString),
            validations(esd.validations)
          )

    }
  }
}
