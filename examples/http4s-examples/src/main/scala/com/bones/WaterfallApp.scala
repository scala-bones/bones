package com.bones

import java.time.ZonedDateTime

import cats.effect.IO
import com.bones.crud.Algebra.ServiceOps
import com.bones.data.Value.KvpNil
import com.bones.fullstack.CrudDbDefinitions.WithId
import com.bones.fullstack.LocalhostAllIOApp
import com.bones.syntax._
import com.bones.validation.ValidationDefinition.{BigDecimalValidation => dv, LongValidation => lv, StringValidation => sv}
import org.http4s.HttpRoutes
import cats.effect._
import cats.implicits._



object WaterfallDefinitions {

  case class Error(error: String)
  val errorDef = (kvp("error", string) :: KvpNil).convert[Error]


  case class Waterfall(name: String, latitude: BigDecimal, longitude: BigDecimal, cubicFeetPerMinute: Option[Long], heightInInches: Option[Long])
  val waterfall = (
    kvp("name", string(sv.alphanum, sv.max(500))) ::
      kvp("latitude", bigDecimal(dv.min(-180), dv.max(180))) ::
      kvp("longitude", bigDecimal(dv.min(-180), dv.max(180))) ::
      kvp("cubicFeetPerMinute", long(lv.positive).optional) ::
      kvp("heightInInches", long(lv.positive).optional) ::
      KvpNil
    ).convert[Waterfall]

  val waterfallWithId = (
    kvp("id", long(lv.min(0))) ::
    waterfall :><:
    KvpNil
  ).convert[WithId[Waterfall]]

  val waterfallService =
    ServiceOps.withPath("waterfall")
      .withCreate(waterfall, waterfallWithId, errorDef)
      .withRead(waterfallWithId, errorDef)
      .withUpdate(waterfall, waterfallWithId, errorDef)
      .withDelete(waterfallWithId, errorDef)

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

  val waterfallVisitWithId = (
    kvp("id", long(lv.min(0))) ::
    waterfallVisit :><:
    KvpNil
  ).convert[WithId[WaterfallVisit]]

  val waterfallVisitService =
    ServiceOps.withPath("waterfallVisit")
      .withCreate(waterfallVisit, waterfallVisitWithId, errorDef)
      .withRead(waterfallVisitWithId, errorDef)
      .withUpdate(waterfallVisit, waterfallVisitWithId, errorDef)
      .withDelete(waterfallVisitWithId, errorDef)

}

object WaterfallApp extends LocalhostAllIOApp() {

  import LocalhostAllIOApp._
  import WaterfallDefinitions._

  val ds = localhostDataSource

  override def services: HttpRoutes[IO] = {
    dbSchemaEndpoint(waterfallService) <+> dbSchemaEndpoint(waterfallVisitService)
  }
}
