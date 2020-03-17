package com.bones.swagger.custom

import com.bones.data.custom._
import com.bones.swagger.SwaggerCoreInterpreter
import com.bones.swagger.SwaggerCoreInterpreter.{CustomSwaggerInterpreter, Name}
import com.bones.validation.ValidationDefinition.ValidationOp
import io.swagger.v3.oas.models.media.{EmailSchema, Schema}

trait CustomStringInterpreter extends CustomSwaggerInterpreter[CustomStringValue] {

  def withSchema[A](vo: CustomStringValue[A]): Schema[_] => Schema[_] =
    vo match {
      case _: HostnameData => _.format("hostname")
      case _: IpV4Data => _.format("ipv4")
      case _: IpV6Data => _.format("ipv6")
      case _: UriData => _.format("uri")
      case _: UrlData => _.format("url")
      case _: GuidData =>
        schema =>
          schema
            .minLength(36)
            .maxLength(36)
            .pattern(
              "(^([0-9A-Fa-typeToConversion]{8}[-][0-9A-Fa-typeToConversion]{4}[-][0-9A-Fa-typeToConversion]{4}[-][0-9A-Fa-typeToConversion]{4}[-][0-9A-Fa-typeToConversion]{12})$)")
            .format("guid")
      case _: CreditCardData => identity
      case _: EmailData =>
        val emailSchema = new EmailSchema()
        schema =>
          emailSchema.setName(schema.getName); emailSchema
      case _: HexStringData => identity
      case _: Base64Data => identity
    }



  override def toSchema[A](alg: CustomStringValue[A], description: Option[String], example: Option[A]):
    Name => SwaggerCoreInterpreter.SwaggerSchemas[Schema[_]] = name => {


    SwaggerCoreInterpreter.addStringSchema(name, alg.customValidation.description, alg.example.toString, withSchema(alg))

  }
}
