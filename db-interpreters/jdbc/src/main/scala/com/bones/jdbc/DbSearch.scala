package com.bones.jdbc

import java.sql.{Connection, ResultSet}

import cats.data.NonEmptyList
import cats.effect.IO
import com.bones.Util.NullableResult
import com.bones.data.Error.{ExtractionError, ExtractionErrors}
import com.bones.data.KvpCollection.headTypeName
import com.bones.data.{KvpCollection, KvpNil}
import com.bones.jdbc.DbUtil.camelToSnake
import com.bones.jdbc.column.ColumnNameInterpreter
import com.bones.jdbc.rs.ResultSetInterpreter
import fs2.Stream
import javax.sql.DataSource

trait DbSearch[ALG[_]] {

  def resultSetInterpreter: ResultSetInterpreter[ALG]
  def columnNameInterpreter: ColumnNameInterpreter[ALG]

  def getEntity[A: Manifest, ID: Manifest](
    schema: KvpCollection[ALG, A],
    idSchema: KvpCollection[ALG, ID]
  ): DataSource => Stream[IO, Either[ExtractionErrors, (ID, A)]] = {
    val withConnection = searchEntityWithConnection(schema, idSchema)
    ds =>
      {
        Stream
          .bracket(IO { ds.getConnection })(con => IO { con.close() })
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

  def searchEntityWithConnection[A: Manifest, ID: Manifest](
    schema: KvpCollection[ALG, A],
    idSchema: KvpCollection[ALG, ID]
  ): Connection => Stream[IO, Either[ExtractionErrors, (ID, A)]] = {
    val tableName = camelToSnake(headTypeName(schema).getOrElse("Unknown"))
    val schemaWithId: KvpCollection[ALG, (ID, A)] =
      (idSchema :: schema :: new KvpNil[ALG]).tupled[(ID, A)]

    val resultSetF: ResultSet => Either[ExtractionErrors, (ID, A)] =
      resultSetInterpreter.generateResultSet[(ID, A)](schemaWithId)(List.empty)

    val fields = columnNameInterpreter.fromKvpCollection(schemaWithId)
    val sql = s"""select ${fields.mkString(",")} from $tableName limit 50"""
    con =>
      {
        for {
          statement <- Stream.bracket(IO { con.prepareCall(sql) })(s => IO { s.close() })
          resultSet <- Stream.bracket(IO { statement.executeQuery() })(s => IO { s.close() })
          a <- extractToStream(resultSet, resultSetF)
        } yield {
          a
        }
      }
  }

}
