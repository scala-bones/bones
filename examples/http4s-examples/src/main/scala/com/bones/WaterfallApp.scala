package com.bones

import cats.effect.IO
import com.bones.data.KvpNil
import com.bones.fullstack.LocalhostAllIOApp
import com.bones.syntax._
import com.bones.validation.ValidationDefinition.{BigDecimalValidation => dv, LongValidation => lv, StringValidation => sv}
import org.http4s.HttpRoutes
import cats.effect._
import cats.implicits._
import com.bones.oas3.SwaggerCoreInterpreter.SwaggerSchemas
import com.bones.schemas.Schemas


object WaterfallDefinitions {

  case class ImperialMeasurement(feet: Long, inches: Long)

  case class Location(latitude: BigDecimal, longitude: BigDecimal)

  val imperialMeasurement = (
    kvp("feet", long(lv.min(0))) ::
      kvp("inches", long(lv.between(0, 12))) ::
      KvpNil
    ).convert[ImperialMeasurement]

  object WaterVolume extends Enumeration {
    type WaterVolume = Value
    val Low, Average, High = Value
  }


  case class Waterfall(name: String, latitude: BigDecimal, longitude: BigDecimal, cubicFeetPerMinute: Option[BigDecimal],
                       height: Option[ImperialMeasurement], waterValue: WaterVolume.Value, // discoveryDate: LocalDateTime,
                       /*wantToVisit: Boolean, description: String*/)

  val waterfallSchema = (
    kvp("name", string(sv.max(200))) ::
      kvp("latitude", bigDecimal(dv.min(-180), dv.max(180))) ::
      kvp("longitude", bigDecimal(dv.min(-180), dv.max(180))) ::
      kvp("cubicFeetPerMinute", bigDecimal(dv.positive).optional) ::
      kvp("height", imperialMeasurement.optional) ::
      kvp("waterVolume", enumeration[WaterVolume.type, WaterVolume.Value](WaterVolume)) ::
      //      kvp("discoveryDate", isoDateTime()) ::
//      kvp("wantToVisit", boolean) ::
//      kvp("description", string(sv.max(500))) ::
      KvpNil
    ).convert[Waterfall]


  case class WaterfallVisit(waterfallId: Long, waterVolume: WaterVolume.Value, notes: Option[String])

  val waterfallVisitSchema = (
    kvp("waterfallId", long(lv.min(1))) ::
      //      kvp("visitDate", isoDate()) ::
      kvp("waterVolume", enumeration[WaterVolume.type, WaterVolume.Value](WaterVolume)) ::
      kvp("notes", string.optional) ::
      KvpNil
    ).convert[WaterfallVisit]

}

object WaterfallApp extends LocalhostAllIOApp() {

  import LocalhostAllIOApp._
  import WaterfallDefinitions._

  val ds = localhostDataSource

  override def services: HttpRoutes[IO] = {
    serviceRoutesWithCrudMiddleware("waterfall", waterfallSchema, ds) <+>
      //    serviceRoutesWithCrudMiddleware(waterfallVisitService, ds) <+>
      dbSchemaEndpoint("waterfall", waterfallSchema) <+>
      dbSchemaEndpoint("waterfallVisit", waterfallVisitSchema) <+>
      serviceRoutesWithCrudMiddleware("allSupported", Schemas.allSupportCaseClass, ds) <+>
      reactEndpoints(List(waterfallSchema, waterfallVisitSchema))
  }
}
