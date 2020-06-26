package com.bones

import cats.effect.IO
import com.bones.data.KvpNil
import com.bones.fullstack.LocalhostAllIOApp
import com.bones.syntax._
import com.bones.validation.ValidationDefinition.{
  BigDecimalValidation => dv,
  StringValidation => sv
}
import org.http4s.HttpRoutes
import cats.effect._
import cats.implicits._

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
      ("location", locationSchema.optional) :<:
      ("height", bigDecimal(dv.min(0)).optional) :<:
      kvpNil
  ).convert[Waterfall]

}

object DemoApp extends LocalhostAllIOApp() {

  import Demo._
  import LocalhostAllIOApp._

  val ds = localhostDataSource

  override def services: HttpRoutes[IO] = {
    serviceRoutesWithCrudMiddleware("waterfall", waterfallSchema, idDef, parseIdF, com.bones.jdbc.defaultJdbcColumnInterpreter, ds) <+>
      dbSchemaEndpoint("waterfall", waterfallSchema)
//      reactEndpoints(List(waterfallSchema))
  }
}
