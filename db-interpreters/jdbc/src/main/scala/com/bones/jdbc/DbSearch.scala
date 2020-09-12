package com.bones.jdbc

import java.sql.{Connection, ResultSet}

import cats.data.NonEmptyList
import cats.effect.IO
import com.bones.data.Error.ExtractionError
import com.bones.data.KvpCollection.headManifest
import com.bones.data.{KvpCollection, KvpCollectionValue, KvpWrappedHList, HigherOrderValue}
import com.bones.jdbc.DbUtil.camelToSnake
import com.bones.jdbc.column.ColumnNameInterpreter
import com.bones.jdbc.rs.{ResultSetInterpreter, ResultSetValue => ResultSetCustomInterpreter}
import fs2.Stream
import fs2.Stream.bracket
import javax.sql.DataSource

trait DbSearch[ALG[_]] {

  def resultSetInterpreter: ResultSetInterpreter[ALG]
  def columnNameInterpreter: ColumnNameInterpreter[ALG]

  def getEntity[A, ID](
    schema: KvpCollection[ALG, A],
    idDef: IdDefinition[ALG, ID]
  ): DataSource => Stream[IO, Either[NonEmptyList[ExtractionError], (ID, A)]] = {
    val withConnection = searchEntityWithConnection(schema, idDef)
    ds =>
      {
        bracket(IO { ds.getConnection })(con => IO { con.close() })
          .flatMap(con => withConnection(con))
      }
  }

  private def extractToStream[A, ID](
    rs: ResultSet,
    resultSetF: ResultSet => Either[NonEmptyList[ExtractionError], (ID, A)])
    : Stream[IO, Either[NonEmptyList[ExtractionError], (ID, A)]] = {
    if (rs.next()) {
      val next = resultSetF(rs)
      Stream(next) ++ extractToStream(rs, resultSetF)
    } else {
      Stream.empty
    }
  }

  def searchEntityWithConnection[A, ID](
    schema: KvpCollection[ALG, A],
    idDef: IdDefinition[ALG, ID]
  ): Connection => Stream[IO, Either[NonEmptyList[ExtractionError], (ID, A)]] = {
    val tableName = camelToSnake(
      headManifest(schema).map(_.runtimeClass.getSimpleName).getOrElse("unknown"))
    val schemaWithId: KvpCollection[ALG, (ID, A)] = idDef.prependSchema(schema)

    val resultSetF: ResultSet => Either[NonEmptyList[ExtractionError], (ID, A)] =
      resultSetInterpreter.fromKvpCollection[(ID, A)](schemaWithId)(List.empty)

    val fields = columnNameInterpreter.fromKvpCollection(schemaWithId)
    val sql = s"""select ${fields.mkString(",")} from $tableName limit 50"""
    con =>
      {
        for {
          statement <- bracket(IO { con.prepareCall(sql) })(s => IO { s.close() })
          resultSet <- bracket(IO { statement.executeQuery() })(s => IO { s.close() })
          a <- extractToStream(resultSet, resultSetF)
        } yield {
          a
        }
      }
  }

}
