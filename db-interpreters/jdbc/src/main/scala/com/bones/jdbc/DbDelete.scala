package com.bones.jdbc

import java.sql.Connection

import cats.data.NonEmptyList
import com.bones.data.Error.{ExtractionError, SystemError}
import com.bones.data.{KvpCollection, HListConvert}
import com.bones.jdbc.DbUtil.{camelToSnake, withDataSource, withStatement}
import com.bones.jdbc.rs.ResultSetValue
import com.bones.jdbc.update.DbUpdateValues
import com.bones.jdbc.update.DbUpdateValues.CustomDbUpdateInterpreter
import javax.sql.DataSource

import scala.util.control.NonFatal

object DbDelete {

  def delete[ALG[_], A, ID](
                             schema: KvpCollection[ALG, A],
                             resultSetCustomInterpreter: ResultSetValue[ALG],
                             idDef: IdDefinition[ALG, ID],
                             customDbUpdateInterpreter: CustomDbUpdateInterpreter[ALG]
  ): DataSource => ID => Either[NonEmptyList[ExtractionError], (ID, A)] = {
    val withConnection =
      deleteWithConnect(schema, resultSetCustomInterpreter, idDef, customDbUpdateInterpreter)
    ds => id =>
      withDataSource(ds)(con => withConnection(id)(con))
  }

  def deleteWithConnect[ALG[_], A, ID](
                                        schema: KvpCollection[ALG, A],
                                        resultSetCustomInterpreter: ResultSetValue[ALG],
                                        idDef: IdDefinition[ALG, ID],
                                        customDbUpdateInterpreter: CustomDbUpdateInterpreter[ALG]
  ): ID => Connection => Either[NonEmptyList[ExtractionError], (ID, A)] = {
    schema match {
      case x: HListConvert[ALG, _, _, _] => {
        val tableName = camelToSnake(x.manifestOfA.runtimeClass.getSimpleName)
        val updateF =
          DbUpdateValues.valueDefinition(idDef.asSchema, customDbUpdateInterpreter)(1, idDef.key)
        val sql = s"delete from ${tableName} where ${updateF.assignmentStatements.map(_._1).mkString(" AND ")}"
        val getEntity = DbGet.getEntityWithConnectionCustomAlgebra(
          schema,
          idDef,
          resultSetCustomInterpreter,
          customDbUpdateInterpreter)
        id => con =>
          {
            try {
              for {
                entity <- getEntity(id)(con)
                _ <- {
                  withStatement[Boolean](con.prepareCall(sql))(statement => {
                    updateF.predicates(id).foreach(_.apply(statement))
                    Right(statement.execute())
                  })
                }
              } yield entity
            } catch {
              case NonFatal(ex) =>
                Left(NonEmptyList.one(SystemError(ex, Some("Error deleting entity"))))
            }
          }
      }
    }
  }

}
