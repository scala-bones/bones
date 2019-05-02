package com.bones.fullstack

import cats.effect.IO
import com.bones.crud.Algebra.ServiceOps
import com.bones.crud.WithId
import com.bones.jdbc._
import fs2.Stream
import javax.sql.DataSource

object CrudDbDefinitions {

  //  def entityWithId[B](entity: BonesSchema[B]): BonesSchema[WithId[B]] =
  //    entity match {
  //      case op: HListConvert[a,al,B] =>
  //        implicit val x = op.manifestOfA
  //        (kvp("id", long(lv.min(0))) ::
  //          op :><:
  //          KvpNil
  //        ).convert[WithId[B]]
  //    }


  //  case class WithId[A](id: Long, a: A)
  case class DbError(message: String)

}

case class CrudDbDefinitions[CI, CO, RO, UI, UO, DO](
                                                      schema: ServiceOps[CI, CO, _, RO, _, UI, UO, _, DO, _],
                                                      ds: DataSource) {

  import CrudDbDefinitions._

  // TODO: deal with error better
  val searchF: Stream[IO, WithId[Long, RO]] =
    schema.readOperation.map(op => {
      DbSearch.getEntity(op.successSchemaForRead)(ds)
        .flatMap(out => {
          out match {
            case Left(errO) => Stream.empty
            case Right(ro) => Stream {
              ro
            }
          }
        })
    }).getOrElse(Stream.empty)

  def createF: CI => IO[Either[DbError, WithId[Long, CI]]] = {
    schema.createOperation.map(op => {
      val insertQuery = DbInsertValues.insertQuery(op.inputSchema)(ds)
      (input: CI) =>
        IO {
          insertQuery(input)
            .left.map(ex => DbError(ex.toString))
        }
    }).getOrElse((input: CI) => IO {
      Left(DbError("Create Not Supported"))
    })
  }

  val readF: Long => IO[Either[DbError, WithId[Long, RO]]] = {
    schema.readOperation.map(op => {
      val readQuery = DbGet.getEntity(op.successSchemaForRead)(ds)
      (id: Long) =>
        IO {
          readQuery(id)
            .left.map(ex => DbError(ex.toString()))
        }
    }).getOrElse((id: Long) => IO {
      Left(DbError("Read Not Supported"))
    })
  }

  val updateF: (Long, UI) => IO[Either[DbError, WithId[Long, UI]]] = {
    schema.updateOperation.map(op => {
      val updateQuery = DbUpdateValues.updateQuery(op.inputSchema)(ds)
      (id: Long, input: UI) =>
        IO {
          updateQuery(id, input)
            .left.map(ex => DbError(ex.toString()))
        }
    }).getOrElse((id: Long, input: UI) => IO {
      Left(DbError("Update not supported"))
    })
  }

  val deleteF: Long => IO[Either[DbError, WithId[Long, DO]]] = {
    schema.deleteOperation.map(op => {
      val deleteQuery = DbDelete.delete(op.successSchema)(ds)
      (id: Long) =>
        IO {
          deleteQuery(id)
            .left.map(ex => DbError(ex.toString()))
        }
    }).getOrElse((id: Long) => IO {
      Left(DbError("Delete not supported "))
    })
  }

}
