package com.bones.http4s

import java.nio.charset.StandardCharsets

import cats.data.NonEmptyList
import cats.effect._
import cats.implicits._
import com.bones.circe.IsoCirceEncoderAndValidatorInterpreter
import com.bones.data.Error.ExtractionError
import com.bones.data.{BonesSchema, HListConvert, KeyValueDefinition}
import com.bones.http4s.ClassicCrudInterpreter.{CustomInterpreter, ProtobufEncoderInterpreter}
import com.bones.interpreter.KvpInterchangeFormatEncoderInterpreter.InterchangeFormatEncoder
import com.bones.interpreter.KvpInterchangeFormatValidatorInterpreter.InterchangeFormatValidator
import com.bones.protobuf.ProtoFileGeneratorInterpreter.Name
import com.bones.protobuf.ProtobufSequentialEncoderInterpreter.EncodeToProto
import com.bones.protobuf.ProtobufSequentialValidatorInterpreter.ExtractFromProto
import com.bones.protobuf._
import com.bones.swagger.SwaggerCoreInterpreter.CustomSwaggerInterpreter
import com.bones.swagger.{CrudOasInterpreter, SwaggerCoreInterpreter}
import com.bones.syntax.NoAlgebra
import fs2.Stream
import io.circe.Json
import io.swagger.v3.oas.models.OpenAPI
import org.http4s._
import org.http4s.dsl.Http4sDsl
import org.http4s.dsl.impl.Path
import reactivemongo.bson.BSONValue

object ClassicCrudInterpreter {

  trait CustomInterpreter[ALG[_], I]
      extends InterchangeFormatValidator[ALG, I]
      with InterchangeFormatEncoder[ALG, I]

  case class NoAlgebraCustomInterpreter[I]() extends CustomInterpreter[NoAlgebra, I] {
    override def encode[A](alg: NoAlgebra[A]): A => I = sys.error("Unreachable code")
    override def validate[A](
      alg: NoAlgebra[A]
    ): (Option[I], List[String]) => Either[NonEmptyList[ExtractionError], A] =
      sys.error("Unreachable code")
  }

  trait ProtobufEncoderInterpreter[ALG[_]]
      extends ProtobufSequentialValidatorInterpreter.CustomValidatorInterpreter[ALG]
      with ProtobufSequentialEncoderInterpreter.CustomEncoderInterpreter[ALG]
      with ProtoFileGeneratorInterpreter.CustomInterpreter[ALG]

  case object NoAlgebraProtobufEncoderInterpreter extends ProtobufEncoderInterpreter[NoAlgebra] {
    override def encodeToProto[A](alg: NoAlgebra[A]): EncodeToProto[A] =
      sys.error("Unreachable code")

    override def extractFromProto[A](alg: NoAlgebra[A]): ExtractFromProto[A] =
      sys.error("Unreachable code")

    override def toMessageField[A](alg: NoAlgebra[A]): (Name, Int) => (
      ProtoFileGeneratorInterpreter.MessageField,
      Vector[ProtoFileGeneratorInterpreter.NestedType],
      Int
    ) =
      sys.error("Unreachable code")
  }

  def emptyCoreAlgebra[A, E, F[_], ID: Manifest](
    path: String,
    charset: java.nio.charset.Charset = StandardCharsets.UTF_8,
    schema: BonesSchema[NoAlgebra, A],
    idDefinition: KeyValueDefinition[NoAlgebra, ID],
    pathStringToId: String => Either[E, ID],
    errorSchema: BonesSchema[NoAlgebra, E]
  )(implicit F: Sync[F], H: Http4sDsl[F]): ClassicCrudInterpreter[NoAlgebra, A, E, F, ID] =
    emptyCustomAlgebra[NoAlgebra, A, E, F, ID](
      path,
      NoAlgebraCustomInterpreter[Json](),
      NoAlgebraCustomInterpreter[BSONValue](),
      NoAlgebraProtobufEncoderInterpreter,
      SwaggerCoreInterpreter.noAlgebraInterpreter,
      schema,
      idDefinition,
      pathStringToId,
      errorSchema,
      charset
    )

  /** Creates a CRUD definition, but no user defined functions, so no actual endpoints.
    * To be used with the [ClassicCrudInterpreter.withCreate], [ClassicCrudInterpreter.withRead],
    *   [ClassicCrudInterpreter.withUpdate], [ClassicCrudInterpreter.withDelete] and [ClassicCrudInterpreter.withSearch].  Use
    *   this method if not all CRUD endpoints should be implemented.
    *   Use the [allVerbs] method if all CRUD endpoints should be implemented.
    *   See [ClassicCrudInterpreter] for details on the parameters.
    */
  def emptyCustomAlgebra[ALG[_], A, E, F[_], ID: Manifest](
    path: String,
    customJsonInterpreter: CustomInterpreter[ALG, Json],
    customBsonInterpreter: CustomInterpreter[ALG, BSONValue],
    customProtobufInterpreter: ProtobufEncoderInterpreter[ALG],
    customSwaggerInterpreter: CustomSwaggerInterpreter[ALG],
    schema: BonesSchema[ALG, A],
    idDefinition: KeyValueDefinition[ALG, ID],
    pathStringToId: String => Either[E, ID],
    errorSchema: BonesSchema[ALG, E],
    charset: java.nio.charset.Charset = StandardCharsets.UTF_8
  )(
    implicit F: Sync[F],
    H: Http4sDsl[F]
  ): ClassicCrudInterpreter[ALG, A, E, F, ID] =
    ClassicCrudInterpreter(
      path,
      customJsonInterpreter,
      customBsonInterpreter,
      customProtobufInterpreter,
      customSwaggerInterpreter,
      schema,
      idDefinition,
      pathStringToId,
      errorSchema,
      None,
      None,
      None,
      None,
      None,
      charset
    )

  def allVerbs[A, E, F[_], ID: Manifest](
    path: String,
    schema: BonesSchema[NoAlgebra, A],
    idDefinition: KeyValueDefinition[NoAlgebra, ID],
    pathStringToId: String => Either[E, ID],
    errorSchema: BonesSchema[NoAlgebra, E],
    createF: A => F[Either[E, (ID, A)]],
    readF: ID => F[Either[E, (ID, A)]],
    updateF: (ID, A) => F[Either[E, (ID, A)]],
    deleteF: ID => F[Either[E, (ID, A)]],
    searchF: () => Stream[F, (ID, A)],
    charset: java.nio.charset.Charset = StandardCharsets.UTF_8
  )(
    implicit F: Sync[F],
    H: Http4sDsl[F]
  ): ClassicCrudInterpreter[NoAlgebra, A, E, F, ID] = allVerbsCustomAlgebra[NoAlgebra, A, E, F, ID](
    path,
    NoAlgebraCustomInterpreter[Json](),
    NoAlgebraCustomInterpreter[BSONValue](),
    NoAlgebraProtobufEncoderInterpreter,
    SwaggerCoreInterpreter.noAlgebraInterpreter,
    schema,
    idDefinition,
    pathStringToId,
    errorSchema,
    createF,
    readF,
    updateF,
    deleteF,
    searchF,
    charset
  )

  /**
    * Creates a CRUD definition so that all CRUD verbs (GET, POST, PUT, DELETE) are supported.  If only a sub-set of Verbs
    * are desired, see [emptyCoreAlgebra].
    *   See [ClassicCrudInterpreter] for details on the parameters.
    */
  def allVerbsCustomAlgebra[ALG[_], A, E, F[_], ID: Manifest](
    path: String,
    customJsonInterpreter: CustomInterpreter[ALG, Json],
    customBsonInterpreter: CustomInterpreter[ALG, BSONValue],
    customProtobufInterpreter: ProtobufEncoderInterpreter[ALG],
    customSwaggerInterpreter: CustomSwaggerInterpreter[ALG],
    schema: BonesSchema[ALG, A],
    idDefinition: KeyValueDefinition[ALG, ID],
    pathStringToId: String => Either[E, ID],
    errorSchema: BonesSchema[ALG, E],
    createF: A => F[Either[E, (ID, A)]],
    readF: ID => F[Either[E, (ID, A)]],
    updateF: (ID, A) => F[Either[E, (ID, A)]],
    deleteF: ID => F[Either[E, (ID, A)]],
    searchF: () => Stream[F, (ID, A)],
    charset: java.nio.charset.Charset = StandardCharsets.UTF_8
  )(
    implicit F: Sync[F],
    H: Http4sDsl[F]
  ): ClassicCrudInterpreter[ALG, A, E, F, ID] = ClassicCrudInterpreter(
    path,
    customJsonInterpreter,
    customBsonInterpreter,
    customProtobufInterpreter,
    customSwaggerInterpreter,
    schema,
    idDefinition,
    pathStringToId,
    errorSchema,
    Some(createF),
    Some(readF),
    Some(updateF),
    Some(deleteF),
    Some(searchF),
    charset
  )
}

/**
  * Builds out CREATE, READ, UPDATE and DELETE endpoints given a bones Schema and some other metadata.
  * @param path The http path to this API
  * @param charset UTF_* is a good choice here.
  * @param schema The bones schema describing the data that is available at this REST endpoint.
  * @param idDefinition The id definition to be used (probably one representing a long, uuid or int)
  * @param pathStringToId Need to be able to convert the path/id from a String to the ID type.  For
  *                       instance, url path GET /object/1 where 1 is the ID to convert.  This comes in as a string.
  * @param errorSchema Used to describe how we want to display System Error data.
  * @param createF User defined function which is called after validating the input value.
  *                Probably used to create the data in data store.
  *                If None, no create endpoint will be created.
  * @param readF User defined function which is called to look up an entity in a data store by id.
  *              if None, no read endpoint will be created
  * @param updateF User defined function which is called after validating the input value to update the data in a data store.
  *                If None, no update endpoint will be created.
  * @param deleteF User defined function to delete data in a data store.
  *                If None, no delete endpoint will be created.
  * @param searchF user defined function to return all entities in a data store.
  *                If None, no search (get all) endpoint will be created.
  *                This needs to be improved to provide search by parameters.
  * @param F Should be an implementation of Sync, IO is a good default choice.
  * @tparam A The class Value of the endpoint defined by the bones Schema.
  * @tparam E The error type of user defined functions.
  * @tparam F An subclass of the Sync type class
  * @tparam ID The ID type.
  */
case class ClassicCrudInterpreter[ALG[_], A, E, F[_], ID: Manifest](
  path: String,
  customJsonInterpreter: CustomInterpreter[ALG, Json],
  customBsonInterpreter: CustomInterpreter[ALG, BSONValue],
  customProtobufInterpreter: ProtobufEncoderInterpreter[ALG],
  customSwaggerInterpreter: CustomSwaggerInterpreter[ALG],
  schema: BonesSchema[ALG, A],
  idDefinition: KeyValueDefinition[ALG, ID],
  pathStringToId: String => Either[E, ID],
  errorSchema: BonesSchema[ALG, E],
  createF: Option[A => F[Either[E, (ID, A)]]] = None,
  readF: Option[ID => F[Either[E, (ID, A)]]] = None,
  updateF: Option[(ID, A) => F[Either[E, (ID, A)]]] = None,
  deleteF: Option[ID => F[Either[E, (ID, A)]]] = None,
  searchF: Option[() => Stream[F, (ID, A)]],
  charset: java.nio.charset.Charset = StandardCharsets.UTF_8,
)(implicit F: Sync[F], H: Http4sDsl[F]) {

  /** Add or overwrite the existing user defined function to create. Adding a create function
    * will ensure the creation of a Create(PUT) endpoint.
    * @param create The function which is to be called during Create input.
    * @return Copy of ClassicCrudInterpreter with the new create function.
    */
  def withCreate(create: A => F[Either[E, (ID, A)]]): ClassicCrudInterpreter[ALG, A, E, F, ID] =
    this.copy(createF = Some(create))

  /** Add or overwrite the existing user defined function to find data in a data store.  Adding
    * a read function will ensure the create of a GET endpoint.
    * @param read The function to be called to look up an entity in a data store.
    * @return Copy of the ClassicCrudInterpreter with the new create function
    */
  def withRead(read: ID => F[Either[E, (ID, A)]]): ClassicCrudInterpreter[ALG, A, E, F, ID] =
    this.copy(readF = Some(read))

  /** Add or overwrite the existing user defined function to update data in a data store.  Adding
    * a update function will ensure the create of a POST endpoint.
    * @param update The function to be called to update an entity in a data store.
    * @return Copy of the ClassicCrudInterpreter with the new update function
    */
  def withRead(update: (ID, A) => F[Either[E, (ID, A)]]): ClassicCrudInterpreter[ALG, A, E, F, ID] =
    this.copy(updateF = Some(update))

  /** Add or overwrite the existing user defined function to delete data in a data store.  Adding
    * a delete function will ensure the create of a POST endpoint.
    * @param delete The function to be called to delete an entity in a data store.
    * @return Copy of the ClassicCrudInterpreter with the new delete function
    */
  def withDelete(delete: ID => F[Either[E, (ID, A)]]): ClassicCrudInterpreter[ALG, A, E, F, ID] =
    this.copy(deleteF = Some(delete))

  /** Add or overwrite the existing user defined function to search for data in a data store.  Adding
    * a search function will ensure the create of a GET endpoint to return all entities.
    * @param search The function to be called to search for all entities in a data store.
    * @return Copy of the ClassicCrudInterpreter with the new delete function
    */
  def withSearch(search: () => Stream[F, (ID, A)]): ClassicCrudInterpreter[ALG, A, E, F, ID] =
    this.copy(searchF = Some(search))
  val schemaWithId = schema match {
    case h: HListConvert[ALG, _, _, A] @unchecked =>
      implicit val manifest: Manifest[A] = h.manifestOfA
      (idDefinition >>: h :><: com.bones.syntax.kvpNilCov[ALG]).tupled[(ID, A)]
  }
  val encodeToCirceInterpreter = IsoCirceEncoderAndValidatorInterpreter
  val validatedFromCirceInterpreter = IsoCirceEncoderAndValidatorInterpreter

  val protobufSequentialInputInterpreter: ProtobufSequentialValidatorInterpreter =
    ProtobufUtcSequentialEncoderAndValidator
  val protobufSequentialOutputInterpreter: ProtobufSequentialEncoderInterpreter =
    ProtobufUtcSequentialEncoderAndValidator

  def createRoutes: HttpRoutes[F] = {

    object http extends BaseCrudInterpreter[ALG, A, E, (ID, A), F, ID]

    val updateHttpService =
      updateF.toList.flatMap(update => {
        BaseCrudInterpreter.updateRoute(
          path,
          pathStringToId,
          update,
          schema,
          errorSchema,
          schemaWithId,
          validatedFromCirceInterpreter,
          encodeToCirceInterpreter,
          protobufSequentialInputInterpreter,
          protobufSequentialOutputInterpreter,
          customJsonInterpreter,
          customBsonInterpreter,
          customProtobufInterpreter,
          charset
        )
      })

    val readHttpService = readF.toList.flatMap(read => {
      BaseCrudInterpreter.httpGetRoute(
        path,
        pathStringToId,
        read,
        encodeToCirceInterpreter,
        errorSchema,
        schemaWithId,
        customJsonInterpreter,
        customBsonInterpreter,
        customProtobufInterpreter,
        protobufSequentialOutputInterpreter,
        charset
      )
    })

    val searchHttpService = searchF.toList.flatMap(search => {
      BaseCrudInterpreter.httpSearch(
        path,
        search,
        encodeToCirceInterpreter,
        errorSchema,
        schemaWithId,
        customJsonInterpreter,
        customBsonInterpreter,
        customProtobufInterpreter,
        protobufSequentialOutputInterpreter,
        charset
      )
    })

    val createHttpService = createF.toList.flatMap(create => {
      BaseCrudInterpreter.httpPostRoutes(
        path,
        pathStringToId,
        create,
        schema,
        errorSchema,
        schemaWithId,
        validatedFromCirceInterpreter,
        encodeToCirceInterpreter,
        protobufSequentialInputInterpreter,
        protobufSequentialOutputInterpreter,
        customJsonInterpreter,
        customBsonInterpreter,
        customProtobufInterpreter,
        charset
      )
    })

    val deleteHttpService = deleteF.toList.flatMap(del => {
      BaseCrudInterpreter.httpDeleteRoutes(
        path,
        pathStringToId,
        del,
        encodeToCirceInterpreter,
        errorSchema,
        schemaWithId,
        customJsonInterpreter,
        customBsonInterpreter,
        customProtobufInterpreter,
        protobufSequentialOutputInterpreter,
        charset,
      )
    })

    val contentTypes = "application/json" :: "application/ubjson" :: "application/protobuf" :: Nil
    val swaggerHtml = swaggerDoc(
      contentTypes,
      customSwaggerInterpreter,
      schema,
      schemaWithId,
      errorSchema,
      path
    )

    val swagger = http.htmlEndpoint(Path(s"swagger/$path"), swaggerHtml)

    val services: List[HttpRoutes[F]] =
      http.protoBuff(path, customProtobufInterpreter, schema, schemaWithId, errorSchema) :: swagger :: createHttpService ::: readHttpService ::: updateHttpService ::: deleteHttpService ::: searchHttpService

    services.foldLeft[HttpRoutes[F]](HttpRoutes.empty)(
      (op1: HttpRoutes[F], op2: HttpRoutes[F]) => op1 <+> op2
    )

  }

  /** Create an endpoint to display the swagger doc for classic Crud this type. */
  def swaggerDoc(
    contentTypes: List[String],
    customInterpreter: CustomSwaggerInterpreter[ALG],
    schema: BonesSchema[ALG, A],
    schemaWithId: BonesSchema[ALG, (ID, A)],
    errorSchema: BonesSchema[ALG, E],
    path: String
  ): String = {

    val openApi =
      CrudOasInterpreter.jsonApiForService(
        path,
        "Rest Service",
        "1.0",
        contentTypes,
        schema,
        schemaWithId,
        errorSchema,
        customInterpreter,
        createF.isDefined,
        readF.isDefined,
        updateF.isDefined,
        deleteF.isDefined,
        searchF.isDefined
      )(new OpenAPI())
    io.swagger.v3.core.util.Json.mapper().writeValueAsString(openApi)

  }

}
