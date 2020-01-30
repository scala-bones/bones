package com.bones.fullstack

import cats.data.NonEmptyList
import cats.effect.IO
import com.bones.data.Error.{ExtractionError, SystemError}
import com.bones.data.BonesSchema
import com.bones.jdbc.JdbcCustomInterpreter
import com.bones.jdbc.NoAlgebraJdbCustomInterpreter
import com.bones.jdbc._
import com.bones.syntax.NoAlgebra
import fs2.Stream
import javax.sql.DataSource

object CrudDbDefinitions {
  def apply[A](schema: BonesSchema[NoAlgebra, A], ds: DataSource): CrudDbDefinitions[NoAlgebra, A] =
    CrudDbDefinitions[NoAlgebra, A](schema, NoAlgebraJdbCustomInterpreter, ds)
}

/**
  * For a given schema, this class provides the basic CRUD operations
  * for manipulating data in a Database.
  * @param schema
  * @param ds
  * @tparam A
  */
case class CrudDbDefinitions[ALG[_], A](
  schema: BonesSchema[ALG, A],
  customInterpreter: JdbcCustomInterpreter[ALG],
  ds: DataSource) {

  import CrudDbDefinitions._

  // TODO: deal with error better
  val searchF: Stream[IO, (Long, A)] =
    DbSearch
      .getEntity(schema, customInterpreter)(ds)
      .flatMap({
        case Left(errO) => Stream.empty
        case Right(ro) =>
          Stream {
            ro
          }
      })

  def createF: A => IO[Either[ExtractionError, (Long, A)]] = {
    val insertQuery = DbInsertValues.insertQuery(schema, customInterpreter)(ds)
    input: A =>
      IO {
        insertQuery(input)
      }
  }

  val readF: Long => IO[Either[NonEmptyList[ExtractionError], (Long, A)]] = {
    val readQuery = DbGet.getEntity(schema, customInterpreter)(ds)
    id: Long =>
      IO {
        readQuery(id)
      }
  }

  val updateF: (Long, A) => IO[Either[NonEmptyList[ExtractionError], (Long, A)]] = {
    val updateQuery = DbUpdateValues.updateQueryCustomAlgebra(schema, customInterpreter)(ds)
    (id: Long, input: A) =>
      IO {
        updateQuery(id, input)
      }
  }

  val deleteF: Long => IO[Either[NonEmptyList[ExtractionError], (Long, A)]] = {
    val deleteQuery = DbDelete.delete(schema, customInterpreter)(ds)
    id: Long =>
      IO {
        deleteQuery(id)
      }
  }

}
