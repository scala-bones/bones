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
  LongValidation => lv,
  StringValidation => sv
}
import org.http4s.HttpRoutes
import org.http4s.headers.`Content-Type`

object WaterfallDefinitions {

  case class ImperialMeasurement(feet: Long, inches: Long)

  case class Location(latitude: BigDecimal, longitude: BigDecimal)

  val imperialMeasurement = (
    ("feet", long(lv.min(0))) ::
      ("inches", long(lv.between(0, 12))) ::
      kvpNil
  ).convert[ImperialMeasurement]

  object WaterVolume extends Enumeration {
    type WaterVolume = Value
    val Low, Average, High = Value
  }

  case class Waterfall(
    name: String,
    latitude: BigDecimal,
    longitude: BigDecimal,
    cubicFeetPerMinute: Option[BigDecimal],
    height: Option[ImperialMeasurement],
    waterValue: WaterVolume.Value // discoveryDate: LocalDateTime,
    /*wantToVisit: Boolean, description: String*/ )

  val waterfallSchema = (
    ("name", string(sv.max(200))) ::
      ("latitude", bigDecimal(dv.min(-180), dv.max(180))) ::
      ("longitude", bigDecimal(dv.min(-180), dv.max(180))) ::
      ("cubicFeetPerMinute", bigDecimal(dv.positive).optional) :<:
      ("height", imperialMeasurement.asValue.optional) :<:
      ("waterVolume", enumeration[WaterVolume.type, WaterVolume.Value](WaterVolume)) ::
      //      kvp("discoveryDate", isoDateTime()) ::
//      kvp("wantToVisit", boolean) ::
//      kvp("description", string(sv.max(500))) ::
      kvpNil
  ).convert[Waterfall]

  case class WaterfallVisit(
    waterfallId: Long,
    waterVolume: WaterVolume.Value,
    notes: Option[String])

  val waterfallVisitSchema = (
    ("waterfallId", long(lv.min(1))) ::
      //      kvp("visitDate", isoDate()) ::
      ("waterVolume", enumeration[WaterVolume.type, WaterVolume.Value](WaterVolume)) ::
      ("notes", string.optional) :<:
      kvpNil
  ).convert[WaterfallVisit]

  val idSchema = (("id", long(lv.positive)) :: kvpNil).encodedHead[Long]()

}

object WaterfallApp extends LocalhostAllIOApp() {

  import LocalhostAllIOApp._
  import WaterfallDefinitions._

  val ds = localhostDataSource

  val interpreterConfig = Config.interpreterConfig
  val crudDef =
    ClassicCrudDef.defaultValues[Waterfall, Long, `Content-Type`, ErrorResponse](
      interpreterConfig,
      "waterfall",
      waterfallSchema,
      Path.longParam,
      ExtractionErrorEncoder.errorResponseSchema.algMapKvpCollection[DefaultValues](core =>
        shapeless.Inl(core)),
      DefaultIdDefinitions.longIdDefinition,
      "id"
    )

  override def services: HttpRoutes[IO] = {
    serviceRoutesWithCrudMiddleware(
      crudDef,
      com.bones.jdbc.select.defaultSelectInterpreter,
      com.bones.jdbc.dbSearchInterpreter,
      com.bones.jdbc.insert.defaultDbInsertInterpreter,
      com.bones.jdbc.update.defaultDbUpdate,
      com.bones.jdbc.dbDeleteInterpreter,
      ds
    ) <+>
      //    serviceRoutesWithCrudMiddleware(waterfallVisitService, ds) <+>
      dbSchemaEndpoint("waterfall", waterfallSchema) <+>
      dbSchemaEndpoint("waterfallVisit", waterfallVisitSchema)
  }
}
