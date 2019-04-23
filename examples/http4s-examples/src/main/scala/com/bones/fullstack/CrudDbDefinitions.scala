package com.bones.fullstack

import cats.effect.IO
import com.bones.crud.Algebra.ServiceOps
import com.bones.data.Value.BonesSchema
import com.bones.jdbc.{DbDelete, DbGet, DbInsertValues, DbUpdateValues}
import fs2.Stream
import javax.sql.DataSource

object CrudDbDefinitions {
  case class WithId[A](id: Long, a: A)
  case class DbError(message: String)
}

case class CrudDbDefinitions[CI, RO, UI, DO](
  schema: ServiceOps[CI, _, _, RO, _, UI, _, _, DO, _],
  ds: DataSource) {

  import CrudDbDefinitions._

  val searchF: Stream[IO, RO] = Stream.empty

  def createF: CI => IO[Either[DbError,WithId[CI]]] = {
    schema.createOperation.map(op => {
      val insertQuery = DbInsertValues.insertQuery(op.inputSchema)(ds)
      (input:CI) => IO {
        insertQuery(input)
          .right.map(pid => WithId(pid._1, pid._2))
          .left.map(ex => DbError(ex.toString))
      }
    }).getOrElse(  (input: CI) => IO { Left(DbError("Create Not Supported"))} )
  }

  val readF: Long => IO[Either[DbError, RO]] = {
    schema.readOperation.map(op => {
      val readQuery = DbGet.getEntity(op.successSchemaForRead)(ds)
      (id: Long) => IO {
        readQuery(id)
          .left.map(ex => DbError(ex.toString()))
      }
    }).getOrElse(  (id: Long) => IO { Left(DbError("Read Not Supported"))} )
  }

  val updateF: (Long, UI) => IO[Either[DbError, WithId[UI]]] = {
    schema.updateOperation.map(op => {
      val updateQuery = DbUpdateValues.updateQuery(op.inputSchema)(ds)
      (id: Long, input: UI) => IO {
        updateQuery(id,input)
          .right.map(pid => WithId(id, pid))
          .left.map(ex => DbError(ex.toString()))
      }
    }).getOrElse( (id: Long, input:UI) => IO { Left(DbError("Update not supported"))})
  }

  val deleteF: Long => IO[Either[DbError, DO]] = {
    schema.deleteOperation.map(op => {
      val deleteQuery = DbDelete.delete(op.successSchema)(ds)
      (id: Long) => IO {
        deleteQuery(id)
          .left.map(ex => DbError(ex.toString()))
      }
    }).getOrElse( (id:Long) => IO { Left(DbError("Delete not supported "))})
  }

}
