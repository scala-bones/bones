package com.bones

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



object Demo {

  //  case class Error(error: String)
  val errorSchema = (kvp("error", string) :: KvpNil).convert[DbError]

  case class Location(latitude: BigDecimal, longitude: BigDecimal)

  case class Waterfall(name: String, location: Option[Location], height: Option[BigDecimal])

  val locationSchema = (
      kvp("latitude", bigDecimal(dv.min(-180), dv.max(180))) ::
      kvp("longitude", bigDecimal(dv.min(-180), dv.max(180))) ::
      KvpNil
    ).convert[Location]

  val waterfallSchema = (
      kvp("name", string(sv.max(200), sv.trimmed, sv.words)) ::
      kvp("location", locationSchema.optional) ::
      kvp("height", bigDecimal(dv.min(0)).optional) ::
      KvpNil
    ).convert[Waterfall]

  val waterfallService =
    ServiceOps
      .havingPath("waterfall")
      .providingCreate(waterfallSchema, waterfallSchema, errorSchema)
      .providingRead(waterfallSchema, errorSchema)
      .providingUpdate(waterfallSchema, waterfallSchema, errorSchema)
      .providingDelete(waterfallSchema, errorSchema)

}

object DemoApp extends LocalhostAllIOApp() {

  import LocalhostAllIOApp._
  import Demo._

  val ds = localhostDataSource

  override def services: HttpRoutes[IO] = {
    serviceRoutesWithCrudMiddleware(waterfallService, ds) <+>
      dbSchemaEndpoint(waterfallService) <+>
      reactEndpoints(List(waterfallSchema))
  }
}







