package com.bones.jdbc

import java.sql.{Connection, ResultSet, SQLException}

import cats.data.NonEmptyList
import com.bones.data.Error.{ExtractionError, SystemError}
import com.bones.data.Value.{BonesSchema, XMapData}
import com.bones.jdbc.DbUtil.{camelToSnake, withStatement}

import scala.util.control.NonFatal
import cats._
import cats.effect.IO
import cats.implicits._
import com.bones.crud.WithId
import javax.sql.DataSource
import fs2.Stream
import fs2.Stream.bracket

object DbSearch {


  def getEntity[A](schema: BonesSchema[A]): DataSource => Stream[IO, Either[NonEmptyList[ExtractionError], WithId[Long,A]]] = {
    val withConnection = searchEntityWithConnection[A](schema)
    ds => {
      bracket( IO { ds.getConnection })(con =>  IO {con.close() })
        .flatMap(con => withConnection(con))
    }
  }

  private  def extractToStream[A](rs: ResultSet, resultSetF: ResultSet => Either[NonEmptyList[ExtractionError],WithId[Long,A]]) : Stream[IO,Either[NonEmptyList[ExtractionError],WithId[Long,A]]] = {
    if (rs.next()) {
      val next = resultSetF(rs)
      Stream( next ) ++ extractToStream(rs, resultSetF)
    } else {
      Stream.empty
    }
  }

  def searchEntityWithConnection[A](schema: BonesSchema[A]): Connection => Stream[IO, Either[NonEmptyList[ExtractionError], WithId[Long,A]]] = {
    schema match {
      case x: XMapData[h,n,b] =>
        val tableName = camelToSnake(x.manifestOfA.runtimeClass.getSimpleName)
        val withId = WithId.entityWithId(DbUtil.longIdKeyValueDef, x)
        val resultSetF: ResultSet => Either[NonEmptyList[ExtractionError],WithId[Long,A]] =
          ResultSetInterpreter.valueDefinition(withId)(List.empty, "")

        val fields = ColumnNameInterpreter.valueDefinition(withId)("")
        val sql = s"""select ${fields.mkString(",")} from $tableName limit 50"""
        con => {
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


}
