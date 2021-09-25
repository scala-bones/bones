package com.bones.http.common

import com.bones.data.KvpCollection
import com.bones.interpreter.encoder.Encoder
import com.bones.interpreter.validator.Validator

/** Define now each interpreter serializes to an Array[Byte]
  * @tparam K
  * @tparam ALG
  */
trait Interpreter[K, ALG[_]] {
  def generateEncoder[A](kvp: KvpCollection[K, ALG, A]): Encoder[ALG, A, Array[Byte]]
  def generateValidator[A](kvp: KvpCollection[K, ALG, A]): Validator[K, ALG, A, Array[Byte]]
}

/** A content type and the interpreter.
  * @param contentType
  *   The media content type.
  * @param encoderInterpreter
  *   Responsible for defining how to convert a KvpCollection into a validation or encoder function.
  * @tparam K
  *   The key, probably String
  * @tparam ALG
  *   The Data Algebra
  * @tparam CT
  *   ContentType probably String.
  */
case class Content[K, ALG[_], CT](contentType: CT, encoderInterpreter: Interpreter[K, ALG])

/** Contained cashed functions to encode data from A to Array[Byte] for the default validator and
  * all supported validators. Provides methods to look up data by content type.
  * @param defaultEncoder
  *   The default content type and function pair.
  * @param supportedEncoders
  *   key/value pairs of each content type and it's validation encoders.
  * @tparam ALG
  *   The Schema Algebra
  * @tparam A
  *   Entity
  * @tparam CT
  *   ContentType
  */
case class ConvertedEncoder[ALG[_], A, CT](
  defaultEncoder: (CT, Encoder[ALG, A, Array[Byte]]),
  supportedEncoders: Map[CT, (CT, Encoder[ALG, A, Array[Byte]])]
) {

  def encoderForContent(ct: CT): (CT, Encoder[ALG, A, Array[Byte]]) =
    if (defaultEncoder._1 == ct) defaultEncoder
    else supportedEncoders.getOrElse(ct, defaultEncoder)

  def encoderForOptionalContent(ctOpt: Option[CT]): (CT, Encoder[ALG, A, Array[Byte]]) =
    ctOpt.fold(defaultEncoder)(encoderForContent)
}

/** Contained cashed functions to validated data from Array[Byte] to an A for the default validator
  * and all supported validators. Provides methods to look up data by content type.
  * @param defaultValidator
  *   The default content type and function pair.
  * @param supportedValidators
  *   key/value pairs of each content type and it's validation function.
  * @tparam ALG
  *   The Schema Algebra
  * @tparam A
  *   Entity
  * @tparam CT
  *   ContentType
  */
case class ConvertedValidator[K, ALG[_], A, CT](
  defaultValidator: (CT, Validator[K, ALG, A, Array[Byte]]),
  supportedValidators: Map[CT, (CT, Validator[K, ALG, A, Array[Byte]])]
) {

  def validatorForContent(ct: CT): Option[(CT, Validator[K, ALG, A, Array[Byte]])] =
    if (defaultValidator._1 == ct) Some(defaultValidator)
    else supportedValidators.get(ct)

  def validatorForOptionalContent(
    ctOpt: Option[CT]
  ): Option[(CT, Validator[K, ALG, A, Array[Byte]])] =
    ctOpt.map(validatorForContent).getOrElse(Some(defaultValidator))

}

/** Given a default content type and a set of other supported content types, this class provides
  * helpers to generate and cache validators and encoders for each content type.
  *
  * @param defaultContentType
  *   Used when no content type is specified in the request. JSON is a good default.
  * @param contentTypes
  *   The list of supported content types. Protobuf, Binary Json and others are good to add.
  * @tparam K
  *   The key type, most likely String.
  * @tparam ALG
  *   The Algebra
  * @tparam CT
  *   The content type. Most likely a string, but some http frameworks use enumerations.
  */
case class ContentInterpreters[K, ALG[_], CT](
  defaultContentType: Content[K, ALG, CT],
  contentTypes: Set[Content[K, ALG, CT]]
) {

  /** All content types indexed by CT (content type) */
  val allContentTypes: Map[CT, Content[K, ALG, CT]] =
    (contentTypes + defaultContentType).map(ct => (ct.contentType, ct)).toMap

  /** If none, return the default, else return the Content associated to the CT (content type) */
  def findWithDefault(ctOption: Option[CT]): Option[Content[K, ALG, CT]] =
    ctOption match {
      case Some(ct) => allContentTypes.get(ct)
      case None     => Some(defaultContentType)
    }

  /** Given a schema, generate the encoders for each content type.
    * @return
    *   Tuple - First is the default Encoder, second are the other supported encoders
    */
  def generateEncoders[A](schema: KvpCollection[K, ALG, A]): ConvertedEncoder[ALG, A, CT] = {
    val defaultEncoder = (
      defaultContentType.contentType,
      defaultContentType.encoderInterpreter.generateEncoder(schema)
    )
    val contentTypeEncoders =
      contentTypes
        .map(c => (c.contentType, (c.contentType, c.encoderInterpreter.generateEncoder(schema))))
        .toMap
    ConvertedEncoder(defaultEncoder, contentTypeEncoders)
  }

  /** Given a schema, generate the encoders for each content type.
    * @return
    *   Tuple - First is the default Encoder, second are the other supported encoders
    */
  def generateValidators[A](schema: KvpCollection[K, ALG, A]): ConvertedValidator[K, ALG, A, CT] = {
    val defaultValidator = (
      defaultContentType.contentType,
      defaultContentType.encoderInterpreter.generateValidator(schema)
    )
    val contentTypeValidators =
      contentTypes
        .map(c => (c.contentType, (c.contentType, c.encoderInterpreter.generateValidator(schema))))
        .toMap
    ConvertedValidator(defaultValidator, contentTypeValidators)
  }

}
