package com.bones.httpcommon

import java.nio.charset.Charset

import com.bones.data.Error.ExtractionErrors
import com.bones.data.KvpCollection
import com.bones.swagger.SwaggerCoreInterpreter
import com.bones.syntax.{int, iv, long, lv, uuid}

trait EncoderInterpreter[ALG[_]] {
  def generateEncoder[A](kvp: KvpCollection[String, ALG, A]): A => Array[Byte]
  def generateValidator[A](kvp: KvpCollection[String, ALG, A])
    : (Array[Byte], Charset) => Either[ExtractionErrors[String], A]
}

case class Content[ALG[_]](typeName: String, encoderInterpreter: EncoderInterpreter[ALG])

object InterpreterConfig {
  val longIdDefinition = long(lv.positive)
  val intIdDefinition = int(iv.positive)
  val uuidIdDefinition = uuid()
}

case class InterpreterConfig[ALG[_], ID](
  defaultContentType: Content[ALG],
  contentTypes: Set[Content[ALG]],
  customSwaggerInterpreter: SwaggerCoreInterpreter[ALG],
  idDefinition: ALG[ID],
  charset: java.nio.charset.Charset
) {
  val allContentTypes: Map[String, Content[ALG]] =
    Map.from((contentTypes + defaultContentType).map(ct => (ct.typeName, ct)))
  def fomContentType(contentType: String): Content[ALG] =
    allContentTypes.getOrElse(contentType, defaultContentType)
}
