package com.bones

import java.util.UUID

import cats.effect.IO
import com.bones.data.Value.{StringData, ValueDefinitionOp}
import doobie.implicits._
import doobie.{Transactor, Update0}
import com.bones.syntax._

import scala.collection.mutable

object CcEndpoint extends App {

  import Schemas._

  val errorDef: ValueDefinitionOp[String] = string

  val db = new mutable.HashMap[UUID,CC]()

  val xa = Transactor.fromDriverManager[IO](
    "org.postgresql.Driver", "jdbc:postgresql:world", "postgres", ""
  )

  val y = xa.yolo
  import y._

  def insert1(name: String, age: Option[Short]): Update0 =
    sql"insert into person (entityName, age) values ($name, $age)".update

  insert1("Alice", Some(12)).run.transact(xa).unsafeRunSync

  insert1("Bob", None).quick.unsafeRunSync

//  val plan = DirectToDoobie.toPlan(paths)
//
//  jetty.Http(5678).filter(new Planify(plan)).run



}
