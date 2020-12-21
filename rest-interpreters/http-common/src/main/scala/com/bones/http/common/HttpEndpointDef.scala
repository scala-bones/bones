package com.bones.http.common

import com.bones.data.KvpCollection
import com.bones.data.values.{DefaultValues, ScalaCoreValue}
import com.bones.interpreter.values.ExtractionErrorEncoder
import com.bones.interpreter.values.ExtractionErrorEncoder.ErrorResponse
import shapeless.Inl

object HttpEndpointDef {

  /**
    * Uses DefaultValues as the Algebra.
    * Uses [[StringToIdError]] as the path error.
    */
  def defaultValues[REQ: Manifest, RES: Manifest, CT, E](
    contentInterpreters: ContentInterpreters[String, DefaultValues, CT],
    requestSchema: KvpCollection[String, DefaultValues, REQ],
    responseSchema: KvpCollection[String, DefaultValues, RES],
    errorSchema: KvpCollection[String, DefaultValues, E]
  ): HttpEndpointDef[DefaultValues, REQ, RES, CT, E, StringToIdError] =
    HttpEndpointDef[DefaultValues, REQ, RES, CT, E, StringToIdError](
      contentInterpreters,
      requestSchema,
      responseSchema,
      errorSchema,
      StringToIdError.stringToIdErrorSchema,
      core => Inl(core))
}

/**
  *
  * @param contentInterpreters Set of interpreters for Content-Type such as JSON or Protobuf.
  * @param requestSchema The expected format to receive.
  * @param responseSchema The expected format to return
  * @param errorSchema Type should be the same as what is returned by the service.
  * @tparam REQ Request Type
  * @tparam RES Response Type
  * @tparam CT Type of the content type.
  * @tparam E Error returned from processing.
  * @tparam PE Path Error.
  * @return
  */
case class HttpEndpointDef[ALG[_], REQ: Manifest, RES: Manifest, CT, E, PE](
  contentInterpreters: ContentInterpreters[String, ALG, CT],
  requestSchema: KvpCollection[String, ALG, REQ],
  responseSchema: KvpCollection[String, ALG, RES],
  errorSchema: KvpCollection[String, ALG, E],
  pathErrorSchema: KvpCollection[String, ALG, PE],
  scvToAlg: ScalaCoreValue[_] => ALG[_]
) {

  val requestSchemaValidators: ConvertedValidator[String, ALG, REQ, CT] =
    contentInterpreters.generateValidators(requestSchema)

  val responseSchemaEncoders: ConvertedEncoder[ALG, RES, CT] =
    contentInterpreters.generateEncoders(responseSchema)

  val errorResponseSchemaEncoders: ConvertedEncoder[ALG, E, CT] =
    contentInterpreters.generateEncoders(errorSchema)

  val errorResponseEncoders: ConvertedEncoder[ALG, ErrorResponse, CT] =
    contentInterpreters.generateEncoders[ErrorResponse](
      ExtractionErrorEncoder.errorResponseSchema.algMapKvpCollection[ALG](scvToAlg))

  val pathErrorEncoder: ConvertedEncoder[ALG, PE, CT] =
    contentInterpreters.generateEncoders[PE](pathErrorSchema)

}
