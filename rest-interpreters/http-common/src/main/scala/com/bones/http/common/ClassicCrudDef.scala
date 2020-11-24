package com.bones.http.common

import com.bones.data.values.{DefaultValues, ScalaCoreValue}
import com.bones.data.{KvpCollection, KvpNil}
import com.bones.interpreter.values.ExtractionErrorEncoder
import com.bones.interpreter.values.ExtractionErrorEncoder.ErrorResponse
import shapeless.{:+:, Inl}

object ClassicCrudDef {
  def defaultValues[A: Manifest, ID: Manifest, CT, E](
    contentInterpreters: ContentInterpreters[String, DefaultValues, CT],
    path: String,
    schema: KvpCollection[String, DefaultValues, A],
    pathStringToId: String => Either[StringToIdError, ID],
    errorSchema: KvpCollection[String, DefaultValues, E],
    idDefinition: DefaultValues[ID],
    idKey: String): ClassicCrudDef[DefaultValues, A, ID, CT, E, StringToIdError] =
    ClassicCrudDef[DefaultValues, A, ID, CT, E, StringToIdError](
      contentInterpreters,
      path,
      schema,
      pathStringToId,
      StringToIdError.stringToIdErrorSchema,
      errorSchema,
      idDefinition,
      idKey,
      core => Inl(core)
    )
}

/**
  * ClassicCrud is when a single entity is available at an endpoint where
  *   1. GET returns the entity
  *    2. PUT update the entity, given an ID
  *    3. POST creates the entity given a entity body (such as JSON)
  *    4. DELETE removes an entity given an ID
  * @tparam CT The content type.  Most likely a string, but some http frameworks use enumerations.
  */
case class ClassicCrudDef[ALG[_], A: Manifest, ID: Manifest, CT, E, PE](
  contentInterpreters: ContentInterpreters[String, ALG, CT],
  path: String,
  schema: KvpCollection[String, ALG, A],
  pathStringToId: String => Either[PE, ID],
  pathErrorSchema: KvpCollection[String, ALG, PE],
  errorSchema: KvpCollection[String, ALG, E],
  idDefinition: ALG[ID],
  idKey: String,
  scvToAlg: ScalaCoreValue[_] => ALG[_]
) {

  val idSchema: KvpCollection[String, ALG, ID] =
    ((idKey, idDefinition) :: new KvpNil[String, ALG]).encodedHead()

  val schemaWithId: KvpCollection[String, ALG, (ID, A)] =
    ((idKey, idDefinition) :: schema :: KvpNil[String, ALG]()).tupled[(ID, A)]

  val requestSchemaValidators: ConvertedValidator[String, ALG, A, CT] =
    contentInterpreters.generateValidators(schema)

  val responseSchemaEncoders: ConvertedEncoder[ALG, A, CT] =
    contentInterpreters.generateEncoders(schema)

  val responseSchemaWithIdEncoder: ConvertedEncoder[ALG, (ID, A), CT] =
    contentInterpreters.generateEncoders(schemaWithId)

  val errorResponseSchemaEncoders: ConvertedEncoder[ALG, E, CT] =
    contentInterpreters.generateEncoders(errorSchema)

  val errorResponseEncoders: ConvertedEncoder[ALG, ErrorResponse, CT] =
    contentInterpreters.generateEncoders[ErrorResponse](
      ExtractionErrorEncoder.errorResponseSchema.algMapKvpCollection[ALG](scvToAlg))

  val pathErrorEncoder: ConvertedEncoder[ALG, PE, CT] =
    contentInterpreters.generateEncoders[PE](pathErrorSchema)

  val idEncoders: ConvertedEncoder[ALG, ID, CT] =
    contentInterpreters.generateEncoders(idSchema)

}
