package com.bones.jdbc

import java.sql.Connection

import cats.data.NonEmptyList
import com.bones.data.Error.{ExtractionError, SystemError}
import com.bones.data.{BonesSchema, HListConvert}
import com.bones.jdbc.ColumnNameInterpreter.{ CustomInterpreter => CustomColumnNameInterpeter}
import com.bones.jdbc.ResultSetInterpreter.{CustomInterpreter => CustomResultSetInterpeter }
import com.bones.jdbc.DbInsertValues.ID
import com.bones.jdbc.DbUtil.{camelToSnake, withDataSource, withStatement}
import javax.sql.DataSource

import scala.util.control.NonFatal

object DbDelete {

  type DbDeleteCustomInterpreter[ALG[_]] = CustomColumnNameInterpeter[ALG] with CustomResultSetInterpeter[ALG]

  def delete[ALG[_], A]
    (
      schema: BonesSchema[ALG, A],
      customInterpreter: DbDeleteCustomInterpreter[ALG]
    ) : DataSource => ID => Either[NonEmptyList[ExtractionError],
                                 (Long, A)] = {
    val withConnection = deleteWithConnect(schema, customInterpreter)
    ds => id =>
      withDataSource(ds)(con => withConnection(id)(con))
  }

  def deleteWithConnect[ALG[_], A]
    (
      schema: BonesSchema[ALG, A],
      customInterpreter: DbDeleteCustomInterpreter[ALG]
    )
    : ID => Connection => Either[NonEmptyList[ExtractionError],
                                 (Long, A)] = {
    schema match {
      case x: HListConvert[ALG, _, _, _] => {
        val tableName = camelToSnake(x.manifestOfA.runtimeClass.getSimpleName)
        val sql = s"delete from ${tableName} where id = ?"
        val getEntity = DbGet.getEntityWithConnectionCustomAlgebra(schema, customInterpreter)
        id => con =>
          {
            try {
              for {
                entity <- getEntity(id)(con)
                _ <- {
                  withStatement[Boolean](con.prepareCall(sql))(statement => {
                    statement.setLong(1, id)
                    Right(statement.execute())
                  })
                }
              } yield entity
            } catch {
              case NonFatal(ex) =>
                Left(
                  NonEmptyList.one(
                    SystemError(ex, Some("Error deleting entity"))))
            }
          }
      }
    }
  }

}
