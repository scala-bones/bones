package com.bones

import cats.effect.IO
import com.bones.data.KvpNil
import com.bones.fullstack.LocalhostAllIOApp
import com.bones.syntax._
import com.bones.validation.ValidationDefinition.{BigDecimalValidation => dv, StringValidation => sv}
import org.http4s.HttpRoutes
import cats.effect._
import cats.implicits._



object Demo {

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

}

object DemoApp extends LocalhostAllIOApp() {

  import Demo._
  import LocalhostAllIOApp._

  val ds = localhostDataSource

  override def services: HttpRoutes[IO] = {
    serviceRoutesWithCrudMiddleware("waterfall", waterfallSchema, ds) <+>
      dbSchemaEndpoint("waterfall", waterfallSchema) <+>
      reactEndpoints(List(waterfallSchema))
  }
}







