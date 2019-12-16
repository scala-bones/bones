package com.bones.jdbc

import java.sql.{Connection, ResultSet}

import cats.data.NonEmptyList
import cats.effect.IO
import com.bones.data.Error.ExtractionError
import com.bones.data.{BonesSchema, HListConvert}
import com.bones.jdbc.ColumnNameInterpreter.{ CustomInterpreter => ColumnNameCustomInterpreter }
import com.bones.jdbc.ResultSetInterpreter.{ CustomInterpreter => ResultSetCustomInterpreter }
import com.bones.jdbc.DbUtil.camelToSnake
import fs2.Stream
import fs2.Stream.bracket
import javax.sql.DataSource

object DbSearch {

  type DbSearchCustomInterpreter[ALG[_]] = ColumnNameCustomInterpreter[ALG] with ResultSetCustommarshall then unmarshallInterpreter[ALG]

  def getEntity[ALG[_], A]
    (
      schema: BonesSchema[ALG, A],
      customInterpreter: DbSearchCustomInterpreter[ALG]
    ): DataSource => Stream[
    IO,
    Either[NonEmptyList[ExtractionError], (Long,A)]] = {
    val withConnection = searchEntityWithConnection[ALG, A](schema, customInterpreter)
    ds =>
      {
        bracket(IO { ds.getConnection })(con => IO { con.close() })
          .flatMap(con => withConnection(con))
      }
  }

  private def extractToStream[A](
      rs: ResultSet,
      resultSetF: ResultSet => Either[NonEmptyList[ExtractionError],
                                      (Long,A)])
    : Stream[IO, Either[NonEmptyList[ExtractionError], (Long,A)]] = {
    if (rs.next()) {
      val next = resultSetF(rs)
      Stream(next) ++ extractToStream(rs, resultSetF)
    } else {
      Stream.empty
    }
  }

  def searchEntityWithConnection[ALG[_], A](
      schema: BonesSchema[ALG, A],
      customInterpreter: DbSearchCustomInterpreter[ALG]
  ): Connection => Stream[IO,Either[NonEmptyList[ExtractionError], (Long,A)]] = {
    schema match {
      case x: HListConvert[ALG, h, n, b] @unchecked =>
        val tableName = camelToSnake(x.manifestOfA.runtimeClass.getSimpleName)
        implicit val manifest: Manifest[b] = x.manifestOfA
        val schemaWithId =
            (DbUtil.longIdKeyValueDef[ALG] >>: x :><: com.bones.syntax.kvpNilCov[ALG]).tupled[(Long,A)]

        val resultSetF: ResultSet => Either[NonEmptyList[ExtractionError],
                                            (Long,A)] =
          ResultSetInterpreter.valueDefinition(schemaWithId, customInterpreter)(List.empty, "")

        val fields = ColumnNameInterpreter.valueDefinition(schemaWithId, customInterpreter)("")
        val sql = s"""select ${fields.mkString(",")} from $tableName limit 50"""
        con =>
          {
            for {
              statement <- bracket(IO { con.prepareCall(sql) })(s =>
                IO { s.close() })
              resultSet <- bracket(IO { statement.executeQuery() })(s =>
                IO { s.close() })
              a <- extractToStream(resultSet, resultSetF)
            } yield {
              a
            }
          }
    }
  }

}
