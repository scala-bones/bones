package com.bones.jdbc

import java.sql.Connection

import cats.data.NonEmptyList
import com.bones.data.Error.{ExtractionError, NotFound, SystemError}
import com.bones.data.Value.{BonesSchema, HListConvert}
import com.bones.jdbc.DbUtil.{camelToSnake, withStatement}
import javax.sql.DataSource

import scala.util.control.NonFatal

object DbGet {

  def getEntity[A](schema: BonesSchema[A])
  : DataSource => Long => Either[NonEmptyList[ExtractionError],
    (Long,A)] = {
    val withConnection = getEntityWithConnection(schema)
    ds => { id => {
      DbUtil.withDataSource(ds)(con => withConnection(id)(con))
    }
    }
  }

  def getEntityWithConnection[A](schema: BonesSchema[A])
  : Long => Connection => Either[NonEmptyList[ExtractionError],
    (Long,A)] = {
    schema match {
      case xMap: HListConvert[a, al, b] => {
        val schemaWithId = schema match {
          case h: HListConvert[_,_,A] =>
            implicit val manifest: Manifest[A] = h.manifestOfA
            (DbUtil.longIdKeyValueDef :: h :><: com.bones.syntax.kvpNil).tupled[(Long,A)]
        }

        id => {
          val tableName = camelToSnake(
            xMap.manifestOfA.runtimeClass.getSimpleName)
          val resultSetF =
            ResultSetInterpreter.valueDefinition(schemaWithId)(List.empty, "")

          val fields = ColumnNameInterpreter.valueDefinition(schemaWithId)("")
          val sql =
            s"select ${fields.mkString(",")} from $tableName where id = ?"
          con => {
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
                    SystemError(th, Some(s"SQL Statement: $sql"))))
            }
          }
        }
      }
    }
  }
}
