package com.bones.fullstack

import cats.data.NonEmptyList
import cats.effect.IO
import com.bones.data.KvpCollection
import com.bones.data.Error.ExtractionError
import com.bones.jdbc.{JdbcColumnInterpreter, _}
import com.bones.jdbc.insert.DbInsertValues
import com.bones.jdbc.update.DbUpdateValues
import fs2.Stream
import javax.sql.DataSource

/**
  * For a given schema, this class provides the basic CRUD operations
  * for manipulating data in a Database.
  * @param schema
  * @param ds
  * @tparam A
  */
case class CrudDbDefinitions[ALG[_], A, ID](
                                             schema: KvpCollection[ALG, A],
                                             customInterpreter: JdbcColumnInterpreter[ALG],
                                             idDef: IdDefinition[ALG, ID],
                                             ds: DataSource) {

  // TODO: deal with error better
  val searchF: Stream[IO, (ID, A)] =
    DbSearch
      .getEntity(schema, customInterpreter.resultSet, idDef)(ds)
      .flatMap({
        case Left(errO) => Stream.empty
        case Right(ro) =>
          Stream {
            ro
          }
      })

  def createF: A => IO[Either[NonEmptyList[ExtractionError], (ID, A)]] = {
    val insertQuery = DbInsertValues.insertQuery(
      schema,
      idDef.asSchema,
      customInterpreter.insert,
      customInterpreter.resultSet)(ds)
    input: A =>
      IO {
        insertQuery(input)
      }
  }

  val readF: ID => IO[Either[NonEmptyList[ExtractionError], (ID, A)]] = {
    val readQuery =
      DbGet.getEntity(schema, idDef, customInterpreter.resultSet, customInterpreter.dbUpdate)(ds)
    id: ID =>
      IO {
        readQuery(id)
      }
  }

  val updateF: (ID, A) => IO[Either[NonEmptyList[ExtractionError], (ID, A)]] = {
    val updateQuery = DbUpdateValues.updateQueryCustomAlgebra(
      schema,
      customInterpreter.dbUpdate,
      customInterpreter.dbColumn,
      idDef)(ds)
    (id: ID, input: A) =>
      IO {
        updateQuery(id, input)
      }
  }

  val deleteF: ID => IO[Either[NonEmptyList[ExtractionError], (ID, A)]] = {
    val deleteQuery =
      DbDelete.delete(schema, customInterpreter.resultSet, idDef, customInterpreter.dbUpdate)(ds)
    id: ID =>
      IO {
        deleteQuery(id)
      }
  }

}
