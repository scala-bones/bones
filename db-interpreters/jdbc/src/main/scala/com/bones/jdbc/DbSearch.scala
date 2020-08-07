package com.bones.jdbc

import java.sql.{Connection, ResultSet}

import cats.data.NonEmptyList
import cats.effect.IO
import com.bones.data.Error.ExtractionError
import com.bones.data.{SwitchEncoding, ConcreteValue}
import com.bones.jdbc.DbUtil.camelToSnake
import com.bones.jdbc.column.ColumnNameInterpreter
import com.bones.jdbc.rs.{ResultSetInterpreter, ResultSetValue => ResultSetCustomInterpreter}
import fs2.Stream
import fs2.Stream.bracket
import javax.sql.DataSource

object DbSearch {

  def getEntity[ALG[_], A, ID](
    schema: ConcreteValue[ALG, A],
    customInterpreter: ResultSetCustomInterpreter[ALG],
    idDef: IdDefinition[ALG, ID]
  ): DataSource => Stream[IO, Either[NonEmptyList[ExtractionError], (ID, A)]] = {
    val withConnection = searchEntityWithConnection(schema, customInterpreter, idDef)
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

  def searchEntityWithConnection[ALG[_], A, ID](
    schema: ConcreteValue[ALG, A],
    customInterpreter: ResultSetCustomInterpreter[ALG],
    idDef: IdDefinition[ALG, ID]
  ): Connection => Stream[IO, Either[NonEmptyList[ExtractionError], (ID, A)]] = {
    schema match {
      case x: SwitchEncoding[ALG, h, n, b] @unchecked =>
        val tableName = camelToSnake(x.manifestOfA.runtimeClass.getSimpleName)
        val schemaWithId = idDef.prependSchema(schema)

        val resultSetF: ResultSet => Either[NonEmptyList[ExtractionError], (ID, A)] =
          ResultSetInterpreter.valueDefinition(schemaWithId, customInterpreter)(List.empty, "")

        val fields = ColumnNameInterpreter.valueDefinition(schemaWithId)("")
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
      case _ => ??? // TODO
    }
  }

}
