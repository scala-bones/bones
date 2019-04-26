package com.bones

import java.time.ZonedDateTime

import cats.effect.IO
import com.bones.crud.Algebra.ServiceOps
import com.bones.data.Value.KvpNil
import com.bones.fullstack.CrudDbDefinitions.DbError
import com.bones.fullstack.LocalhostAllIOApp
import com.bones.syntax._
import com.bones.validation.ValidationDefinition.{BigDecimalValidation => dv, LongValidation => lv, StringValidation => sv}
import org.http4s.HttpRoutes
import cats.effect._
import cats.implicits._



object WaterfallDefinitions {

//  case class Error(error: String)
  val errorDef = (kvp("error", string) :: KvpNil).convert[DbError]


  case class ImperialMeasurement(feet: Long, inches: Long)
  val imperialMeasurement = (
    kvp("feet", long(lv.min(0))) ::
    kvp("inches", long(lv.between(0,12))) ::
    KvpNil
  ).convert[ImperialMeasurement]

  case class Waterfall(name: String, latitude: BigDecimal, longitude: BigDecimal, cubicFeetPerMinute: Option[BigDecimal], height: Option[ImperialMeasurement])
  val waterfall = (
    kvp("name", string(sv.max(500))) ::
      kvp("latitude", bigDecimal(dv.min(-180), dv.max(180))) ::
      kvp("longitude", bigDecimal(dv.min(-180), dv.max(180))) ::
      kvp("cubicFeetPerMinute", bigDecimal(dv.positive).optional) ::
      kvp("height", imperialMeasurement.optional) ::
      KvpNil
    ).convert[Waterfall]

  val waterfallService =
    ServiceOps.withPath("waterfall")
      .withCreate(waterfall, waterfall, errorDef)
      .withRead(waterfall, errorDef)
      .withUpdate(waterfall, waterfall, errorDef)
      .withDelete(waterfall, errorDef)

  object WaterVolume extends Enumeration {
    type WaterVolume = Value
    val Low, Average, High = Value
  }


  case class WaterfallVisit(waterfallId: Long, visitDate: ZonedDateTime, waterVolume: WaterVolume.WaterVolume, notes: Option[String])
  val waterfallVisit = (
    kvp("waterfallId", long(lv.min(1))) ::
      kvp("visitDate", isoDate()) ::
      kvp("waterVolume", enumeration[WaterVolume.WaterVolume](WaterVolume)) ::
      kvp("notes", string.optional) ::
      KvpNil
    ).convert[WaterfallVisit]

  val waterfallVisitService =
    ServiceOps.withPath("waterfallVisit")
      .withCreate(waterfallVisit, waterfallVisit, errorDef)
      .withRead(waterfallVisit, errorDef)
      .withUpdate(waterfallVisit, waterfallVisit, errorDef)
      .withDelete(waterfallVisit, errorDef)

}

object WaterfallApp extends LocalhostAllIOApp() {

  import LocalhostAllIOApp._
  import WaterfallDefinitions._

  val ds = localhostDataSource

  override def services: HttpRoutes[IO] = {
    serviceRoutesWithCrudMiddleware(waterfallService, ds) <+>
//    serviceRoutesWithCrudMiddleware(waterfallVisitService, ds) <+>
      dbSchemaEndpoint(waterfallService) <+>
      dbSchemaEndpoint(waterfallVisitService)
  }
}
