package com.bones.fullstack

import cats.effect.IO
import com.bones.data.Error.ExtractionErrors
import com.bones.data.KvpCollection
import com.bones.interpreter.values.ExtractionErrorEncoder.ErrorResponse
import com.bones.jdbc._
import com.bones.jdbc.insert.DbInsert
import com.bones.jdbc.select.SelectInterpreter
import com.bones.jdbc.update.DbUpdate
import fs2.Stream
import javax.sql.DataSource

/**
  * For a given schema, this class provides the basic CRUD operations
  * for manipulating data in a Database.
  * @param schema
  * @param ds
  * @tparam A
  */
case class CrudDbDefinitions[ALG[_], A: Manifest, ID: Manifest](
  schema: KvpCollection[String, ALG, A],
  idDef: KvpCollection[String, ALG, ID],
  dbGet: SelectInterpreter[ALG],
  dbSearch: DbSearch[ALG],
  insert: DbInsert[ALG],
  update: DbUpdate[ALG],
  dbDelete: DbDelete[ALG],
  ds: DataSource) {

  // TODO: deal with error better
  val searchF: Stream[IO, (ID, A)] = {
    dbSearch
      .getEntity(schema, idDef)(manifest[A], manifest[ID])(ds)
      .flatMap({
        case Left(errO) => Stream.empty
        case Right(ro) =>
          Stream {
            ro
          }
      })
  }

  val createF: A => IO[Either[ErrorResponse, ID]] = {
    val insertQuery = insert.insertQuery(schema, idDef)(ds)
    input: A =>
      IO {
        insertQuery(input).left.map(ErrorResponse)
      }
  }

  val readF: ID => IO[Either[ErrorResponse, (ID, A)]] = { id: ID =>
    val getF =
      dbGet.selectEntity(schema, idDef)
    IO {
      DbUtil
        .withDataSource(ds)(con => {
          getF(id)(con)
        })
        .left
        .map(ErrorResponse)
    }
  }

  val updateF: (ID, A) => IO[Either[ErrorResponse, ID]] = {
    val updateF = update.updateQuery(schema, idDef)

    (id: ID, input: A) =>
      IO {
        DbUtil
          .withDataSource(ds)(con => {
            updateF(id, input)(con)
          })
          .left
          .map(ErrorResponse)
      }
  }

  val deleteF: ID => IO[Either[ErrorResponse, ID]] = {
    val deleteQuery =
      dbDelete.delete(schema, idDef)
    id: ID =>
      IO {
        DbUtil
          .withDataSource(ds)(con => {
            deleteQuery(id)(con)
          })
          .left
          .map(ErrorResponse)
      }
  }

}
