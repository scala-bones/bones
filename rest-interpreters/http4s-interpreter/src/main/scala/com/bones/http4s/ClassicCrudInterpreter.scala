package com.bones.http4s

import java.nio.charset.StandardCharsets

import cats.data.{EitherT, NonEmptyList}
import cats.effect._
import cats.implicits._
import com.bones.bson.{BsonEncoderInterpreter, BsonValidatorInterpreter}
import com.bones.circe.{CirceEncoderInterpreter, CirceValidatorInterpreter}
import com.bones.data.Error.ExtractionError
import com.bones.data.KeyValueDefinition
import com.bones.data.{BonesSchema, HListConvert}
import com.bones.oas3.CrudOasInterpreter
import com.bones.protobuf.{ProtoFileInterpreter, ProtobufSequentialInputInterpreter, ProtobufSequentialOutputInterpreter}
import fs2.Stream
import io.circe.Json
import io.circe.syntax._
import io.swagger.v3.oas.models.OpenAPI
import org.http4s.circe._
import org.http4s.dsl.Http4sDsl
import shapeless.{::, HNil, Nat, Succ}
import shapeless._
//import org.http4s.dsl.io._
import org.http4s._
import org.http4s.util.CaseInsensitiveString

object ClassicCrudInterpreter {

  /** Creates a CRUD definition, but no user defined functions, so no actual endpoints.
    * To be used with the [ClassicCrudInterpreter.withCreate], [ClassicCrudInterpreter.withRead],
    *   [ClassicCrudInterpreter.withUpdate], [ClassicCrudInterpreter.withDelete] and [ClassicCrudInterpreter.withSearch].  Use
    *   this method if not all CRUD endpoints should be implemented.
    *   Use the [allVerbs] method if all CRUD endpoints should be implemented.
    *   See [ClassicCrudInterpreter] for details on the parameters.
    */
  def empty[A,E,F[_],ID:Manifest](
    path: String,
    charset: java.nio.charset.Charset = StandardCharsets.UTF_8,
    schema: BonesSchema[A],
    idDefinition: KeyValueDefinition[ID],
    pathStringToId: String => Either[E,ID],
    errorSchema: BonesSchema[E],
  )(
    implicit F: Sync[F]
  ): ClassicCrudInterpreter[A,E,F,ID] = ClassicCrudInterpreter(path, charset, schema, idDefinition, pathStringToId, errorSchema, None, None, None, None, None)

  /**
    * Creates a CRUD definition so that all CRUD verbs (GET, POST, PUT, DELETE) are supported.  If only a sub-set of Verbs
    * are desired, see [empty].
    *   See [ClassicCrudInterpreter] for details on the parameters.
    */
  def allVerbs[A,E,F[_],ID:Manifest](
     path: String,
     charset: java.nio.charset.Charset = StandardCharsets.UTF_8,
     schema: BonesSchema[A],
     idDefinition: KeyValueDefinition[ID],
     pathStringToId: String => Either[E,ID],
     errorSchema: BonesSchema[E],
     createF: A => F[Either[E, (ID,A)]],
     readF: ID => F[Either[E,(ID,A)]],
     updateF: (ID,A) => F[Either[E,(ID,A)]],
     deleteF: ID => F[Either[E,(ID,A)]],
     searchF: () => Stream[F,(ID,A)]
   )(
     implicit F: Sync[F]
   ) = ClassicCrudInterpreter(path, charset, schema, idDefinition, pathStringToId, errorSchema, Some(createF), Some(readF), Some(updateF), Some(deleteF), Some(searchF))
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
  *                If None, no udpdate endpoint will be created.
  * @param deleteF User defined function to delete data in a data store.
  *                If None, no delete endpoint will be created.
  * @param searchF user defined function to return all entities in a data store.
  *                If None, no search (get all) endpoint will be created.
  *                This needs to be improved to provide search by parameters.
  * @param F Should be an implementation of Sync, IO is a good default choice.
  * @tparam A The class Value of the endpoint defined by the bones Schema.
  * @tparam E The error type of user defined functions.
  * @tparam F An subclass of the Sync typeclass
  * @tparam ID The ID type.
  */
case class ClassicCrudInterpreter[A,E,F[_], ID:Manifest](
                                                 path: String,
                                                 charset: java.nio.charset.Charset = StandardCharsets.UTF_8,
                                                 schema: BonesSchema[A],
                                                 idDefinition: KeyValueDefinition[ID],
                                                 pathStringToId: String => Either[E,ID],
                                                 errorSchema: BonesSchema[E],
                                                 createF: Option[A => F[Either[E, (ID,A)]]] = None,
                                                 readF: Option[ID => F[Either[E,(ID,A)]]] = None,
                                                 updateF: Option[(ID,A) => F[Either[E,(ID,A)]]] = None,
                                                 deleteF: Option[ID => F[Either[E,(ID,A)]]] = None,
                                                 searchF: Option[() => Stream[F, (ID,A)]]
                                         )(
                              implicit F: Sync[F]
                            ) {

  /** Add or overwrite the existing user defined function to create. Adding a create function
    * will ensure the creation of a Create(PUT) endpoint.
    * @param create The function which is to be called during Create input.
    * @return Copy of ClassicCrudInterpreter with the new create function.
    */
  def withCreate(create: A => F[Either[E, (ID,A)]]) : ClassicCrudInterpreter[A,E,F,ID] =
    this.copy(createF = Some(create))

  /** Add or overwrite the existing user defined function to find data in a data store.  Adding
    * a read function will ensure the create of a GET endpoint.
    * @param read The function to be called to look up an entity in a data store.
    * @return Copy of the ClassicCrudInterpreter with the new create function
    */
  def withRead(read: ID => F[Either[E,(ID,A)]]) : ClassicCrudInterpreter[A,E,F,ID] =
    this.copy(readF = Some(read))

  /** Add or overwrite the existing user defined function to update data in a data store.  Adding
    * a update function will ensure the create of a POST endpoint.
    * @param update The function to be called to update an entity in a data store.
    * @return Copy of the ClassicCrudInterpreter with the new update function
    */
  def withRead(update: (ID,A) => F[Either[E,(ID,A)]]) : ClassicCrudInterpreter[A,E,F,ID] =
    this.copy(updateF = Some(update))

  /** Add or overwrite the existing user defined function to delete data in a data store.  Adding
    * a delete function will ensure the create of a POST endpoint.
    * @param delete The function to be called to delete an entity in a data store.
    * @return Copy of the ClassicCrudInterpreter with the new delete function
    */
  def withDelete(delete: ID => F[Either[E,(ID,A)]]) : ClassicCrudInterpreter[A,E,F,ID] =
    this.copy(deleteF = Some(delete))

  /** Add or overwrite the existing user defined function to search for data in a data store.  Adding
    * a search function will ensure the create of a GET endpoint to return all entities.
    * @param search The function to be called to search for all entities in a data store.
    * @return Copy of the ClassicCrudInterpreter with the new delete function
    */
  def withSearch(search: () => Stream[F, (ID,A)]) : ClassicCrudInterpreter[A,E,F,ID] =
    this.copy(searchF = Some(search))


  import ClassicCrudInterpreterDescription._
  val schemaWithId = schema match {
    case h: HListConvert[_,_,A] => {
      implicit val manifest = h.manifestOfA
      (idDefinition :: h :><: com.bones.syntax.kvpNil).tupled[(ID,A)]
    }
  }
  val encodeToCirceInterpreter = CirceEncoderInterpreter.isoInterpreter
  val validatedFromCirceInterpreter = CirceValidatorInterpreter.isoInterpreter

  case class DataTransformation[I, O, E](description: String,
                                         f: I => Either[E, O])


  def createRoutes: HttpRoutes[F] = {

    object http extends ClassicCrudInterpreterF[A,E,F,ID]

    val updateHttpService =
      updateF.toList.flatMap(update => {
        val inputValidation =
          validatedFromCirceInterpreter.fromSchema(schemaWithId)
        val outputEncoder = encodeToCirceInterpreter.fromSchema(schemaWithId)
        val errorEncoder = encodeToCirceInterpreter.fromSchema(errorSchema)

        val json = PutPostInterpreterGroup[(ID,A), (ID,A), E](
          "application/json",
          bytes =>
            validatedFromCirceInterpreter
              .fromByteArray(bytes, charset)
              .flatMap(json => inputValidation(json)),
          uo => outputEncoder(uo).spaces2.getBytes(charset),
          ue => errorEncoder(ue).spaces2.getBytes(charset)
        )

        val bInputValidation =
          BsonValidatorInterpreter.fromSchema(schemaWithId)
        val bOutputEncoder = BsonEncoderInterpreter.fromSchema(schemaWithId)
        val bErrorEncoder = BsonEncoderInterpreter.fromSchema(errorSchema)

        val bson = PutPostInterpreterGroup[(ID,A), (ID,A), E](
          "application/ubjson",
          byte =>
            BsonValidatorInterpreter
              .fromByteArray(byte)
              .flatMap(bjson => bInputValidation(bjson)),
          uo => BsonEncoderInterpreter.bsonResultToBytes(bOutputEncoder(uo)),
          ue => BsonEncoderInterpreter.bsonResultToBytes(bErrorEncoder(ue))
        )

        val pInputInterpreter =
          ProtobufSequentialInputInterpreter.fromBytes(schemaWithId)
        val pOutputEncoder = ProtobufSequentialOutputInterpreter.encodeToBytes(
          schemaWithId)
        val protobufErrorEncoder = ProtobufSequentialOutputInterpreter.encodeToBytes(
          errorSchema)

        val protoBuf = PutPostInterpreterGroup[(ID,A), (ID,A), E](
          "application/protobuf",
          bytes => pInputInterpreter(bytes),
          uo => pOutputEncoder(uo),
          ue => protobufErrorEncoder(ue)
        )

        http.put(path, json, pathStringToId, update) ::
          http.put(path, bson, pathStringToId, update) ::
          http.put(path, protoBuf, pathStringToId, update) ::
          Nil
      })

    val readHttpService = readF.toList.flatMap(read => {
      val outputF =
        encodeToCirceInterpreter.fromSchema(schemaWithId)
      val errorF = encodeToCirceInterpreter.fromSchema(errorSchema)
      val json = GetInterpreterGroup[(ID,A), E](
        "application/json",
        ro => outputF(ro).spaces2.getBytes(charset),
        re => errorF(re).spaces2.getBytes(charset)
      )

      val bOutputF = BsonEncoderInterpreter.fromSchema(schemaWithId)
      val bErrorF = BsonEncoderInterpreter.fromSchema(errorSchema)
      val bson = GetInterpreterGroup[(ID,A), E](
        "application/ubjson",
        ro => BsonEncoderInterpreter.bsonResultToBytes(bOutputF(ro)),
        re => BsonEncoderInterpreter.bsonResultToBytes(bErrorF(re))
      )

      val pOutputF = ProtobufSequentialOutputInterpreter.encodeToBytes(schemaWithId)
      val pErrorF =
        ProtobufSequentialOutputInterpreter.encodeToBytes(errorSchema)
      val protoBuf = GetInterpreterGroup[(ID,A), E](
        "application/protobuf",
        pOutputF,
        pErrorF
      )



      http.get(path, json, pathStringToId, read) ::
        http.get(path, bson, pathStringToId, read) ::
        http.get(path, protoBuf, pathStringToId, read) ::
        Nil
    })

    val searchHttpService =
      searchF.toList.flatMap(search => {
        val outputF =
          encodeToCirceInterpreter.fromSchema(schemaWithId)

        val jsonSearch = SearchInterpreterGroup[F,(ID,A)](
          "application/json",
          ro => {
            Stream("[".getBytes(charset)) ++
              ro.map(out => outputF(out).asJson.noSpaces.getBytes(charset))
                .intersperse(",".getBytes(charset)) ++
              Stream("]".getBytes(charset))
          }
        )
        http.search(path, jsonSearch, search) :: Nil
      })

    val createHttpService =
      createF.toList.flatMap(create => {
        val inputF =
          validatedFromCirceInterpreter.fromSchema(schema)
        val outputF = encodeToCirceInterpreter.fromSchema(schemaWithId)
        val errorF = encodeToCirceInterpreter.fromSchema(errorSchema)

        val json = PutPostInterpreterGroup[A, (ID,A), E](
          "application/json",
          bytes =>
            validatedFromCirceInterpreter
              .fromByteArray(bytes, charset)
              .flatMap(json => inputF(json)),
          uo => outputF(uo).spaces2.getBytes(charset),
          ue => errorF(ue).spaces2.getBytes(charset)
        )

        val bInputF =
          BsonValidatorInterpreter.fromSchema(schema)
        val bOutputF = BsonEncoderInterpreter.fromSchema(schemaWithId)
        val bErrorF = BsonEncoderInterpreter.fromSchema(errorSchema)

        val bson = PutPostInterpreterGroup[A, (ID,A), E](
          "application/ubjson",
          byte =>
            BsonValidatorInterpreter
              .fromByteArray(byte)
              .flatMap(bjson => bInputF(bjson)),
          co => BsonEncoderInterpreter.bsonResultToBytes(bOutputF(co)),
          ce => BsonEncoderInterpreter.bsonResultToBytes(bErrorF(ce))
        )

        val pInputF =
          ProtobufSequentialInputInterpreter.fromBytes(schema)
        val pOutputF = ProtobufSequentialOutputInterpreter.encodeToBytes(
          schemaWithId)
        val pErrorF =
          ProtobufSequentialOutputInterpreter.encodeToBytes(errorSchema)
        val protoBuf = PutPostInterpreterGroup[A, (ID,A), E](
          "application/protobuf",
          pInputF,
          pOutputF,
          pErrorF
        )

        http.post(path, json, create) :: http.post(path, bson, create) :: http.post(
          path,
          protoBuf,
          create) :: Nil

      })

    val deleteHttpService =
      deleteF.toList.flatMap(del => {
        val outputF = encodeToCirceInterpreter.fromSchema(schemaWithId)
        val errorF = encodeToCirceInterpreter.fromSchema(errorSchema)
        val json = DeleteInterpreterGroup[(ID,A), E](
          "application/json",
          dout => outputF(dout).spaces2.getBytes(charset),
          de => errorF(de).spaces2.getBytes(charset)
        )

        val bOutputF = BsonEncoderInterpreter.fromSchema(schemaWithId)
        val bErrorF = BsonEncoderInterpreter.fromSchema(errorSchema)
        val bson = DeleteInterpreterGroup[(ID,A), E](
          "application/ubjson",
          dout => BsonEncoderInterpreter.bsonResultToBytes(bOutputF(dout)),
          de => BsonEncoderInterpreter.bsonResultToBytes(bErrorF(de))
        )

        val pOutputF =
          ProtobufSequentialOutputInterpreter.encodeToBytes(schemaWithId)
        val pErrorF =
          ProtobufSequentialOutputInterpreter.encodeToBytes(errorSchema)
        val protobuf = DeleteInterpreterGroup[(ID,A), E](
          "application/protobuf",
          pOutputF,
          pErrorF
        )

        http.delete(path, json, pathStringToId, del) ::
          http.delete(path, bson, pathStringToId, del) ::
          http.delete(path, protobuf, pathStringToId, del) ::
          Nil
      })

    val contentTypes = "application/json" :: "application/ubjson" :: "application/protobuf" :: Nil
    val swagger = http.swaggerDoc(contentTypes, schema, schemaWithId, errorSchema, path)

    val services: List[HttpRoutes[F]] =
      http.protoBuff(path, schema, schemaWithId, errorSchema) :: swagger :: createHttpService ::: readHttpService ::: updateHttpService ::: deleteHttpService ::: searchHttpService

    services.foldLeft[HttpRoutes[F]](HttpRoutes.empty)(
      (op1: HttpRoutes[F], op2: HttpRoutes[F]) => op1 <+> op2
    )

  }

}

object ClassicCrudInterpreterDescription {
  /** Collection of items used to to create a Post endpoint in this HttpInterpreter. */
  case class PutPostInterpreterGroup[UI, UO, UE](
                                                  contentType: String,
                                                  inInterpreter: Array[Byte] => Either[NonEmptyList[ExtractionError], UI],
                                                  outInterpreter: UO => Array[Byte],
                                                  errorInterpreter: UE => Array[Byte]
                                                )

  /** Collection of items used to to get a Post endpoint in this HttpInterpreter. */
  case class GetInterpreterGroup[RO, RE](
                                          contentType: String,
                                          outInterpreter: RO => Array[Byte],
                                          errorInterpreter: RE => Array[Byte]
                                        )

  /** Collection of items used to to create a Delete endpoint in this HttpInterpreter. */
  case class DeleteInterpreterGroup[DO, DE](
                                             contentType: String,
                                             outInterpreter: DO => Array[Byte],
                                             errorInterpreter: DE => Array[Byte]
                                           )

  /** Collection of items used to to create a Search endpoint in this HttpInterpreter. */
  case class SearchInterpreterGroup[F[_],SO](
                                         contentType: String,
                                         outInterpreter: Stream[F, SO] => Stream[F, Array[Byte]]
                                       )
}

trait ClassicCrudInterpreterF[A,E,F[_],ID] extends Http4sDsl[F] {

  import ClassicCrudInterpreterDescription._

  def eeToOut(ee: NonEmptyList[ExtractionError])(implicit F: Sync[F]): F[Response[F]] = {

    val errors = ee.map(_.toString)
    BadRequest(Json.obj(("success", "false".asJson), ("errors", errors.asJson)))
  }

  /** Create an endpoint to display the swagger doc for for this type.  Endpoint is /swagger/'entityName' */
  def swaggerDoc(
                  contentTypes: List[String],
                  schema: BonesSchema[A],
                  schemaWithId: BonesSchema[(ID,A)],
                  errorSchema: BonesSchema[E],
                  path: String)(implicit F: Sync[F]): HttpRoutes[F] = {

    val openApi =
      CrudOasInterpreter.jsonApiForService(
        path,
        "Rest Service",
        "1.0",
        contentTypes,
        schema,
        schemaWithId,
        errorSchema,
        true,
        true,
        true,
        true,
        true)(
        new OpenAPI())
    val html = io.swagger.v3.core.util.Json.mapper().writeValueAsString(openApi)

    HttpRoutes.of[F] {
      case GET -> Root / "swagger" / path =>
        Ok(html, Header("Content-Type", "text/html"))(F,implicitly[EntityEncoder[F,String]])
    }
  }

  /** Create an endpoint to display the protobuf schema for each endpoint */
  def protoBuff(path: String, schema: BonesSchema[A], schemaWithId: BonesSchema[(ID,A)], errorSchema: BonesSchema[E])(
                       implicit F: Sync[F]): HttpRoutes[F] = {
    def toFile[A] =
      ProtoFileInterpreter.fromSchemaToProtoFile(_: BonesSchema[A])

    val text =
        s"""
           | // Base Schema, Used for input on Create Only
           | ${toFile(schema)}
           |
           | // Base Schema with ID, Used for other CRUD operations besides input on Create
           | ${toFile(schemaWithId)}
           |
           | // Error Output Message
           | ${toFile(errorSchema)}
           |
        """.stripMargin

    HttpRoutes.of[F] {
      case GET -> Root / "proto" / path =>
        Ok(text, Header("Content-Type", "text/plain"))
    }

  }

  /** Crate delete routes from interpreter group */
  def delete(
                      path: String,
                      interpreterGroup: DeleteInterpreterGroup[(ID,A), E],
                      stringParamToId: String => Either[E,ID],
                      deleteF: ID => F[Either[E, (ID,A)]]
                    )(implicit F: Sync[F]): HttpRoutes[F] = HttpRoutes.of[F] {
    case req@Method.DELETE -> Root / path / idParam => {
      val result = for {
        id <- EitherT.fromEither[F] {
          stringParamToId(idParam)
        }
        entity <- EitherT[F, E, (ID,A)] {
          deleteF(id)
        }
      } yield
        Ok(interpreterGroup.outInterpreter(entity),
          Header("Content-Type", interpreterGroup.contentType))

      result.value.flatMap(either => {
        either.left.map( de =>
          InternalServerError(interpreterGroup.errorInterpreter(de),
            Header("Content-Type",interpreterGroup.contentType))
        ).merge
      })
    }
  }

  /** Get content type from the Request Headers if it exists */
  def contentType(req: Request[F]): Option[String] =
    req.headers
      .find(header => header.name == CaseInsensitiveString("Content-Type"))
      .map(_.value)

  /** Create search endpoints form the Interpreter Group */
  def search[CO](
                  path: String,
                  interpreterGroup: SearchInterpreterGroup[F,CO],
                  searchF: () => fs2.Stream[F, CO]
                )(implicit F: Sync[F]): HttpRoutes[F] = {
    HttpRoutes.of[F] {
      case req@Method.GET -> Root / path
        if contentType(req).contains(interpreterGroup.contentType) => {
        Ok(
          interpreterGroup.outInterpreter(searchF()),
          Header("Content-Type", "application/json")
        )
      }
    }
  }

  /** Crate the post endpoint from the Interpreter Group */
  def post[CI, CO, CE](
                        path: String,
                        interpreterGroup: PutPostInterpreterGroup[CI, CO, CE],
                        createF: CI => F[Either[CE, CO]]
                      )(implicit F: Sync[F]): HttpRoutes[F] =
    HttpRoutes.of[F] {
      case req@Method.POST -> Root / path
        if contentType(req).contains(interpreterGroup.contentType) => {
        val result: EitherT[F, F[Response[F]], F[Response[F]]] = for {
          body <- EitherT[F, F[Response[F]], Array[Byte]] {
            req.as[Array[Byte]].map(Right(_))
          }
          in <- EitherT.fromEither[F] {
            interpreterGroup.inInterpreter(body).left.map(x => eeToOut(x))
          }
          out <- EitherT[F, F[Response[F]], CO] {
            createF(in)
              .map(_.left.map(ce => {
                val out = interpreterGroup.errorInterpreter(ce)
                InternalServerError(out,
                  Header("Content-Type",
                    interpreterGroup.contentType))
              }))
          }
        } yield {
          Ok(interpreterGroup.outInterpreter(out),
            Header("Content-Type", interpreterGroup.contentType))
        }
        result.value.flatMap(_.merge)
      }
    }

  /**
    * Create a get endpoint.
    */
  def get(path: String,
                        interpreterGroup: GetInterpreterGroup[(ID,A), E],
                        stringParamToId: String => Either[E,ID],
                        readF: ID => F[Either[E, (ID,A)]])(
                         implicit F: Sync[F]): HttpRoutes[F] = {
    HttpRoutes.of[F] {
      case req@Method.GET -> Root / path / idParam
        if contentType(req).contains(interpreterGroup.contentType) => {
        stringParamToId(idParam)
          .left.map(re => {
            val entityEncoder = implicitly[EntityEncoder[F, Array[Byte]]]
            BadRequest.apply[Array[Byte]](interpreterGroup.errorInterpreter(re),
              Header("Content-Type", interpreterGroup.contentType))(F, entityEncoder)
          })
          .map(id => {
            readF(id)
              .flatMap({
                case Left(re) =>
                  val entityEncoder = implicitly[EntityEncoder[F, Array[Byte]]]
                  BadRequest.apply[Array[Byte]](interpreterGroup.errorInterpreter(re),
                    Header("Content-Type", interpreterGroup.contentType))(F, entityEncoder)
                case Right(ro) =>
                  Ok(interpreterGroup.outInterpreter(ro),
                    Header("Content-Type", interpreterGroup.contentType))
              })
          })
          .merge
      }
    }
  }

  /**
    * Create a PUT endpoint given serialization functors and business logic.
    *
    * @param interpreterGroup contains functions to and from Array[Byte]
    * @param updateF          Business logic to execute after
    * @return
    */
  def put(
                             path: String,
                             interpreterGroup: PutPostInterpreterGroup[(ID,A), (ID,A), E],
                             stringToId: String => Either[E,ID],
                             updateF: (ID,A) => F[Either[E, (ID,A)]]
                           )(implicit F: Sync[F]): HttpRoutes[F] = {

    HttpRoutes.of[F] {
      case req@Method.PUT -> Root / path / idParam
        if contentType(req).contains(interpreterGroup.contentType) => {
        val result: EitherT[F, F[Response[F]], F[Response[F]]] = for {
          body <- EitherT[F, F[Response[F]], Array[Byte]] {
            req.as[Array[Byte]].map(Right(_))
          }
          id <- EitherT.fromEither[F] {
            stringToId(idParam)
              .left.map(e => BadRequest(interpreterGroup.errorInterpreter(e), Header("Content-Type", interpreterGroup.contentType)))
          }
          in <- EitherT.fromEither[F] {
            interpreterGroup.inInterpreter(body).left.map(x => eeToOut(x))
          }
          out <- EitherT[F, F[Response[F]], (ID,A)] {
            updateF.tupled(in).map(_.left.map(ce =>
              InternalServerError(interpreterGroup.errorInterpreter(ce))))
          }
        } yield {
          Ok(interpreterGroup.outInterpreter(out),
            Header("Content-Type", interpreterGroup.contentType))
        }
        result.value.flatMap(_.merge)
      }
    }
  }
}
