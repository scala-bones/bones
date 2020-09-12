package com.bones.fullstack

import cats.data.NonEmptyList
import cats.effect.IO
import com.bones.data.Error.ExtractionError
import com.bones.data.KvpCollection
import com.bones.jdbc._
import com.bones.jdbc.insert.DbInsert
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
case class CrudDbDefinitions[ALG[_], A, ID](
  schema: KvpCollection[ALG, A],
  idDef: IdDefinition[ALG, ID],
  dbGet: DbGetInterpreter[ALG],
  dbSearch: DbSearch[ALG],
  insert: DbInsert[ALG],
  update: DbUpdate[ALG],
  dbDelete: DbDelete[ALG],
  ds: DataSource) {

  // TODO: deal with error better
  val searchF: Stream[IO, (ID, A)] = {
    dbSearch
      .getEntity(schema, idDef)(ds)
      .flatMap({
        case Left(errO) => Stream.empty
        case Right(ro) =>
          Stream {
            ro
          }
      })
  }

  def createF: A => IO[Either[NonEmptyList[ExtractionError], (ID, A)]] = {
    val insertQuery = insert.insertQuery(schema, idDef.asSchema)(ds)
    input: A =>
      IO {
        insertQuery(input).map(x => (x._1.head, x._2))
      }
  }

  val readF: ID => IO[Either[NonEmptyList[ExtractionError], (ID, A)]] = { id: ID =>
    val getF =
      dbGet.getEntity(schema, idDef)
    IO {
      DbUtil.withDataSource(ds)(con => {
        getF(id)(con)
      })
    }
  }

  val updateF: (ID, A) => IO[Either[NonEmptyList[ExtractionError], (ID, A)]] = {
    val updateF = update.updateQuery(schema, idDef)

    (id: ID, input: A) =>
      IO {
        DbUtil.withDataSource(ds)(con => {
          updateF(id, input)(con)
        })
      }
  }

  val deleteF: ID => IO[Either[NonEmptyList[ExtractionError], (ID, A)]] = {
    val deleteQuery =
      dbDelete.delete(schema, idDef)
    id: ID =>
      IO {
        DbUtil.withDataSource(ds)(con => {
          deleteQuery(id)(con)
        })
      }
  }

}
