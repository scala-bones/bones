package com.bones

import java.util.UUID

import cats.effect.IO
import com.bones.data.Algebra.{DataDefinitionOp, StringData}
import com.bones.rest.Algebra.{Create, Read}
import com.bones.rest.unfiltered.DirectToDoobie
import com.bones.rest.unfiltered.DirectToDoobie.DoobieInfo
import doobie.{Transactor, Update0}
import doobie.util.transactor.Transactor.Aux
import unfiltered.filter.Planify
import doobie._
import doobie.implicits._


import scala.collection.mutable

object CcEndpoint extends App {

  import Schemas._

  val errorDef: DataDefinitionOp[String] = StringData()

  val db = new mutable.HashMap[UUID,CC]()

  val doobieCc = new DoobieInfo[CC] {
    override def transactor: Aux[IO, Unit] = ???

    override def get(id: Long): doobie.ConnectionIO[CC] = ???

    override def insert(a: CC): doobie.ConnectionIO[Int] = ???
  }

  val xa = Transactor.fromDriverManager[IO](
    "org.postgresql.Driver", "jdbc:postgresql:world", "postgres", ""
  )

  val y = xa.yolo
  import y._

  def insert1(name: String, age: Option[Short]): Update0 =
    sql"insert into person (name, age) values ($name, $age)".update

  insert1("Alice", Some(12)).run.transact(xa).unsafeRunSync

  insert1("Bob", None).quick.unsafeRunSync


  import unfiltered.jetty

//  val plan = DirectToDoobie.toPlan(paths)
//
//  jetty.Http(5678).filter(new Planify(plan)).run



}
