package com.bones.httpcommon

import java.nio.charset.Charset

import com.bones.data.Error.ExtractionErrors
import com.bones.data.KvpCollection

trait EncoderInterpreter[ALG[_]] {
  def generateEncoder[K, A](kvp: KvpCollection[K, ALG, A]): EncoderFunc[A]
  def generateValidator[K, A](kvp: KvpCollection[K, ALG, A]): ValidatorFunc[A]
}

case class Content[ALG[_], CT](contentType: CT, encoderInterpreter: EncoderInterpreter[ALG])

case class GeneratedEncoder[CT, A](ct: CT, f: A => Array[Byte])
case class GeneratedValidator[CT, A](
  ct: CT,
  f: (Array[Byte], Charset) => Either[ExtractionErrors[String], A])

case class InterpreterConfig[ALG[_], ID, CT](
  defaultContentType: Content[ALG, CT],
  contentTypes: Set[Content[ALG, CT]],
  idDefinition: ALG[ID],
  charset: java.nio.charset.Charset
) {

  val allContentTypes: Map[CT, Content[ALG, CT]] =
    Map.from((contentTypes + defaultContentType).map(ct => (ct.contentType, ct)))

  def findWithDefault(ctOption: Option[CT]): Option[Content[ALG, CT]] =
    ctOption match {
      case Some(ct) => allContentTypes.get(ct)
      case None     => Some(defaultContentType)
    }

  def generateEncoders[K, A](
    schema: KvpCollection[K, ALG, A]): ((CT, EncoderFunc[A]), Map[CT, EncoderFunc[A]]) = {
    val defaultEncoder = (
      defaultContentType.contentType,
      defaultContentType.encoderInterpreter.generateEncoder(schema))
    val contentTypeEncoders =
      contentTypes.map(c => (c.contentType, c.encoderInterpreter.generateEncoder(schema))).toMap
    (defaultEncoder, contentTypeEncoders)
  }

  def generateValidators[K, A](
    schema: KvpCollection[K, ALG, A]): ((CT, ValidatorFunc[A]), Map[CT, ValidatorFunc[A]]) = {
    val defaultValidator = (
      defaultContentType.contentType,
      defaultContentType.encoderInterpreter.generateValidator(schema)
    )
    val contentTypeValidators =
      contentTypes.map(c => (c.contentType, c.encoderInterpreter.generateValidator(schema))).toMap
    (defaultValidator, contentTypeValidators)
  }

}
