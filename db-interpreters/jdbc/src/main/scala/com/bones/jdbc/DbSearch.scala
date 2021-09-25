package com.bones.jdbc

import java.sql.{Connection, ResultSet}

import cats.effect.IO
import com.bones.data.Error.ExtractionErrors
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
    schema: KvpCollection[String, ALG, A],
    idSchema: KvpCollection[String, ALG, ID]
  ): DataSource => Stream[IO, Either[ExtractionErrors[String], (ID, A)]] = {
    val withConnection = searchEntityWithConnection(schema, idSchema)
    ds => {
      Stream
        .bracket(IO { ds.getConnection })(con => IO { con.close() })
        .flatMap(con => withConnection(con))
    }
  }

  private def extractToStream[A, ID](
    rs: ResultSet,
    resultSetF: ResultSet => Either[ExtractionErrors[String], (ID, A)]
  ): Stream[IO, Either[ExtractionErrors[String], (ID, A)]] = {
    if (rs.next()) {
      val next = resultSetF(rs)
      Stream(next) ++ extractToStream(rs, resultSetF)
    } else {
      Stream.empty
    }
  }

  def searchEntityWithConnection[A: Manifest, ID: Manifest](
    schema: KvpCollection[String, ALG, A],
    idSchema: KvpCollection[String, ALG, ID]
  ): Connection => Stream[IO, Either[ExtractionErrors[String], (ID, A)]] = {
    val tableName = camelToSnake(headTypeName(schema).getOrElse("Unknown"))
    val schemaWithId: KvpCollection[String, ALG, (ID, A)] =
      (idSchema :: schema :: new KvpNil[String, ALG]).tupled[(ID, A)]

    val resultSetF: ResultSet => Either[ExtractionErrors[String], (ID, A)] =
      resultSetInterpreter.generateResultSet[(ID, A)](schemaWithId)(List.empty)

    val fields = columnNameInterpreter.fromKvpCollection(schemaWithId)
    val sql = s"""select ${fields.mkString(",")} from $tableName limit 50"""
    con => {
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
