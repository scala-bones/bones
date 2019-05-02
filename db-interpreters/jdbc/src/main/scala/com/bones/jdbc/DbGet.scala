package com.bones.jdbc

import java.sql.Connection

import cats.data.NonEmptyList
import com.bones.crud.WithId
import com.bones.data.Error.{ExtractionError, NotFound, SystemError}
import com.bones.data.Value.{BonesSchema, HListConvert}
import com.bones.jdbc.DbUtil.{camelToSnake, withStatement}
import javax.sql.DataSource

import scala.util.control.NonFatal

object DbGet {

  def getEntity[A](schema: BonesSchema[A])
    : DataSource => Long => Either[NonEmptyList[ExtractionError],
                                   WithId[Long, A]] = {
    val withConnection = getEntityWithConnection(schema)
    ds =>
      { id =>
        {
          DbUtil.withDataSource(ds)(con => withConnection(id)(con))
        }
      }
  }

  def getEntityWithConnection[A](schema: BonesSchema[A])
    : Long => Connection => Either[NonEmptyList[ExtractionError],
                                   WithId[Long, A]] = {
    schema match {
      case xMap: HListConvert[a, al, b] => {
        val x = WithId.entityWithId[Long, A](DbUtil.longIdKeyValueDef, schema)
        id =>
          {
            val tableName = camelToSnake(
              xMap.manifestOfA.runtimeClass.getSimpleName)
            val resultSetF =
              ResultSetInterpreter.valueDefinition(x)(List.empty, "")

            val fields = ColumnNameInterpreter.valueDefinition(x)("")
            val sql =
              s"""select ${fields.mkString(",")} from $tableName where idDefinition = ?"""
            con =>
              {
                try {
                  withStatement(con.prepareCall(sql))(statement => {
                    statement.setLong(1, id)
                    val rs = statement.executeQuery()
                    if (rs.next()) {
                      resultSetF(rs)
                    } else {
                      Left(
                        NonEmptyList.one(
                          NotFound(id,
                                   xMap.manifestOfA.runtimeClass.getSimpleName,
                                   List.empty)))
                    }
                  })
                } catch {
                  case NonFatal(th) =>
                    Left(
                      NonEmptyList.one(
                        SystemError(List.empty,
                                    th,
                                    Some(s"SQL Statement: $sql"))))
                }
              }
          }
      }
    }

  }

}
