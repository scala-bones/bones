package com.bones.httpcommon

import com.bones.data.KvpCollection
import com.bones.data.values.{DefaultValues, ScalaCoreValue}
import com.bones.interpreter.values.ExtractionErrorEncoder
import com.bones.interpreter.values.ExtractionErrorEncoder.ErrorResponse
import shapeless.Inl

/**
  * Creates the Encoders and Validators for the Request, Response and Error responses.
  * Creates convenience function in order to retrieve a encoder/validator by Content Type (CT).
  * @param interpreterConfig
  * @param requestSchema Schema used to generate Validator to process request data.
  * @param responseSchema Schema used to generate Encoder to create response data.
  * @param errorResponseSchema Schema used to generate Encoder to create response data for the error case.
  * @tparam ALG
  * @tparam REQ
  * @tparam RES
  * @tparam ERR
  * @tparam ID
  * @tparam CT
  */
case class HttpData[ALG[_], REQ, RES, ERR, ID, CT, K](
  interpreterConfig: InterpreterConfig[ALG, ID, CT],
  requestSchema: KvpCollection[K, ALG, REQ],
  responseSchema: KvpCollection[K, ALG, RES],
  errorResponseSchema: KvpCollection[K, ALG, ERR],
  scvToAlg: ScalaCoreValue[_] => ALG[_]
) {

  val (defaultValidator, supportedValidators): (
    (CT, ValidatorFunc[REQ]),
    Map[CT, ValidatorFunc[REQ]]) =
    interpreterConfig.generateValidators(requestSchema)
  val (defaultEncoder, supportedEncoders): ((CT, EncoderFunc[RES]), Map[CT, EncoderFunc[RES]]) =
    interpreterConfig.generateEncoders(responseSchema)
  val (defaultErrorEncoder, supportedErrorEncoder): (
    (CT, EncoderFunc[ERR]),
    Map[CT, EncoderFunc[ERR]]) =
    interpreterConfig.generateEncoders(errorResponseSchema)

  val (defaultExtractionErrorEncoder, supportedExtractionErrorEncoder): (
    (CT, EncoderFunc[ErrorResponse]),
    Map[CT, EncoderFunc[ErrorResponse]]) =
    interpreterConfig.generateEncoders[String, ErrorResponse](
      ExtractionErrorEncoder.errorResponseSchema.algMapKvpCollection[ALG](scvToAlg))

  def validatorForContent(ctOpt: Option[CT]): Option[(CT, ValidatorFunc[REQ])] = {
    ctOpt match {
      case Some(ct) =>
        if (ct == defaultValidator._1) Some((defaultValidator._1, defaultValidator._2))
        else supportedValidators.get(ct).map((ct, _))
      case None => Some((defaultValidator._1, defaultValidator._2))
    }
  }

  def encoderForContent(ct: CT): (CT, EncoderFunc[RES]) =
    if (defaultEncoder._1 == ct) (ct, defaultEncoder._2)
    else supportedEncoders.get(ct).map((ct, _)).getOrElse((defaultEncoder._1, defaultEncoder._2))

  def errorEncoderForContent(ct: CT): (CT, EncoderFunc[ERR]) =
    if (defaultErrorEncoder._1 == ct) (ct, defaultErrorEncoder._2)
    else
      supportedErrorEncoder
        .get(ct)
        .map((ct, _))
        .getOrElse((defaultErrorEncoder._1, defaultErrorEncoder._2))

  def extractionErrorEncoderForContent(ct: CT): (CT, EncoderFunc[ErrorResponse]) =
    if (defaultExtractionErrorEncoder._1 == ct) (ct, defaultExtractionErrorEncoder._2)
    else
      supportedExtractionErrorEncoder
        .get(ct)
        .map((ct, _))
        .getOrElse((defaultExtractionErrorEncoder._1, defaultExtractionErrorEncoder._2))
}
