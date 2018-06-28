package com.bones

import java.util.UUID

import com.bones.data.Algebra.{DataDefinitionOp, StringData}
import com.bones.rest.Sugar.endPoint
import com.bones.rest.unfiltered.UnfilteredRestInterpreter
import javax.servlet.http.{HttpServletRequest, HttpServletResponse}
import unfiltered.filter.Planify
import unfiltered.kit.Routes
import unfiltered.request.GET

import scala.collection.mutable

object CcEndpoint extends App {

  import Schemas._

  val errorDef: DataDefinitionOp[String] = StringData()

  val db = new mutable.HashMap[UUID,CC]()


  val save: CC => Either[String,CC] = cc => db.put(cc.uuid, cc).toRight(s"CC with uuid ${cc.uuid} already exists")
  val get: UUID => Option[CC] = uuid => db.get(uuid)

  val unfilteredDb = new UnfilteredRestInterpreter.Db {
    override def loadById[A: Manifest](uuid: UUID): Option[A] = db.get(uuid).asInstanceOf[Option[A]]

    override def save[A: Manifest, E, R](uuid: UUID, a: A): Either[E, R] = {
      db.put(uuid, a.asInstanceOf[CC])
      Right(a.asInstanceOf[R])
    }

  }
  val x = UnfilteredRestInterpreter(unfilteredDb)


  //Rest test
  val service =
    endPoint[CC]("/creditCard")
      .post[CC,String](creditCardSchema, creditCardSchema, errorDef)
      .get[CC](creditCardSchema)
  //        .put("/:uuid", creditCardSchema, postToProcessor, successShape, errorShape)
  //        .delete("/:uuid", doDelete, successShape, errorShape)


  val paths = x.apply(service)

  import unfiltered.jetty
  import unfiltered.filter

  val plan = UnfilteredRestInterpreter.toPlan(paths)

  jetty.Http(5678).filter(new Planify(plan)).run



}
