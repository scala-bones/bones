package com.bones.jdbc

import java.sql.Connection

import cats.data.NonEmptyList
import com.bones.data.Error.{ExtractionError, SystemError}
import com.bones.data.Value.{BonesSchema, XMapData}
import com.bones.jdbc.DbUtil.{camelToSnake, withStatement}
import javax.sql.DataSource

import scala.util.control.NonFatal

object DbGet {

  def getEntity[A](schema: BonesSchema[A]): DataSource => Long => Either[NonEmptyList[ExtractionError], A] = {
    val withConnection = getEntityWithConnection(schema)
    ds => {
      id => {
        DbUtil.withDataSource(ds)(con => withConnection(id)(con))
      }
    }
  }

  def getEntityWithConnection[A](schema: BonesSchema[A]): Long => Connection => Either[NonEmptyList[ExtractionError], A] = {
    schema match {
      case x: XMapData[h,n,b] =>
        id => {
          val tableName = camelToSnake(x.manifestOfA.runtimeClass.getSimpleName)
          val resultSetF = ResultSetInterpreter.valueDefinition(x)(List.empty, "")

          val fields = ColumnNameInterpreter.valueDefinition(x)("")
          val sql = s"""select ${fields.mkString(",")} from $tableName where id = ?"""
          con => {
            try {
              withStatement(con.prepareCall(sql))(statement => {
                statement.setLong(1,id)
                val rs = statement.executeQuery()
                rs.next()
                resultSetF(rs).asInstanceOf[Either[NonEmptyList[SystemError], A]]
              })
            } catch {
              case NonFatal(th) => Left(NonEmptyList.one(SystemError(List.empty, th, Some(s"SQL Statement: $sql"))))
            }
          }
        }
    }
  }

}
