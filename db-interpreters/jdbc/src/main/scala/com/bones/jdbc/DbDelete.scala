package com.bones.jdbc

import java.sql.Connection

import cats.data.NonEmptyList
import com.bones.crud.WithId
import com.bones.data.Error.{ExtractionError, SystemError}
import com.bones.data.KeyValueDefinition
import com.bones.data.Value.{BonesSchema, HListConvert}
import com.bones.jdbc.DbInsertValues.ID
import com.bones.jdbc.DbUtil.{camelToSnake, withDataSource, withStatement}
import javax.sql.DataSource

import scala.util.control.NonFatal

object DbDelete {

  def delete[A](schema: BonesSchema[A]): DataSource => ID => Either[NonEmptyList[ExtractionError], WithId[Long,A]] = {
    val withConnection = deleteWithConnect(schema)
    ds => id =>
      withDataSource(ds)(con => withConnection(id)(con))
  }

  def deleteWithConnect[A](schema: BonesSchema[A]): ID => Connection => Either[NonEmptyList[ExtractionError], WithId[Long,A]] = {
    schema match {
      case x: HListConvert[_,_,_] => {
        val tableName = camelToSnake(x.manifestOfA.runtimeClass.getSimpleName)
        val sql = s"delete from ${tableName} where id = ?"
        val getEntity = DbGet.getEntityWithConnection(schema)
        id => con => {
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
            case NonFatal(ex) => Left(NonEmptyList.one(SystemError(List.empty, ex, Some("Error deleting entity"))))
          }
        }
      }
    }
  }

}
