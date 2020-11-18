package com.bones

import cats.effect.IO
import cats.implicits._
import com.bones.data.values.DefaultValues
import com.bones.fullstack.LocalhostAllIOApp
import com.bones.http.common.{ClassicCrudDef, DefaultIdDefinitions, Path, StringToIdError}
import com.bones.interpreter.values.ExtractionErrorEncoder
import com.bones.interpreter.values.ExtractionErrorEncoder.ErrorResponse
import com.bones.syntax._
import com.bones.validation.ValidationDefinition.{
  BigDecimalValidation => dv,
  StringValidation => sv
}
import org.http4s.HttpRoutes

object Demo {

  case class Location(latitude: BigDecimal, longitude: BigDecimal)

  case class Waterfall(name: String, location: Option[Location], height: Option[BigDecimal])

  val locationSchema = (
    ("latitude", bigDecimal(dv.min(-180), dv.max(180))) ::
      ("longitude", bigDecimal(dv.min(-180), dv.max(180))) ::
      kvpNil
  ).convert[Location]

  val waterfallSchema = (
    ("name", string(sv.max(200), sv.trimmed, sv.words)) ::
      ("location", locationSchema.asValue.optional) :<:
      ("height", bigDecimal(dv.min(0)).optional) :<:
      kvpNil
  ).convert[Waterfall]

  val idSchema = (("id", long(lv.positive)) :: kvpNil).encodedHead[Long]()

}

object DemoApp extends LocalhostAllIOApp() {

  import Demo._
  import LocalhostAllIOApp._

  val ds = localhostDataSource

  val interpreterConfig = Config.interpreterConfig
  val crudDef =
    ClassicCrudDef[DefaultValues, Waterfall, Long, String, ErrorResponse, StringToIdError](
      interpreterConfig,
      "waterfall",
      waterfallSchema,
      Path.longParam,
      StringToIdError.stringToIdErrorSchema,
      ExtractionErrorEncoder.errorResponseSchema.algMapKvpCollection[DefaultValues](core =>
        shapeless.Inl(core)),
      DefaultIdDefinitions.longIdDefinition,
      "id",
      core => shapeless.Inl(core)
    )

  override def services: HttpRoutes[IO] = {
    serviceRoutesWithCrudMiddleware[Waterfall, Long](
      crudDef,
      com.bones.jdbc.select.defaultSelectInterpreter,
      com.bones.jdbc.dbSearchInterpreter,
      com.bones.jdbc.insert.defaultDbInsertInterpreter,
      com.bones.jdbc.update.defaultDbUpdate,
      com.bones.jdbc.dbDeleteInterpreter,
      ds
    ) <+>
      dbSchemaEndpoint("waterfall", waterfallSchema)
//      reactEndpoints(List(waterfallSchema))
  }
}
