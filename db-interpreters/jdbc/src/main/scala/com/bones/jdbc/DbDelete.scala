package com.bones.jdbc

import java.sql.Connection

import cats.data.NonEmptyList
import com.bones.data.Error.{ExtractionError, SystemError}
import com.bones.data.{Switch, ConcreteValue}
import com.bones.jdbc.DbUtil.{camelToSnake, withDataSource, withStatement}
import com.bones.jdbc.rs.ResultSetValue
import com.bones.jdbc.update.{DbUpdateValue, DbUpdate}
import javax.sql.DataSource

import scala.util.control.NonFatal

object DbDelete {

  /**
   * Creates a curried function which takes an ID and a connection
   * and then deletes the entity in the database with the specified id.
   * @param schema
   * @param resultSetCustomInterpreter
   * @param idDef
   * @param customDbUpdateInterpreter
   * @tparam ALG
   * @tparam A
   * @tparam ID
   * @return
   */
  def delete[ALG[_], A, ID](
                             schema: ConcreteValue[ALG, A],
                             resultSetCustomInterpreter: ResultSetValue[ALG],
                             idDef: IdDefinition[ALG, ID],
                             customDbUpdateInterpreter: DbUpdateValue[ALG]
  ): ID => Connection => Either[NonEmptyList[ExtractionError], (ID, A)] = {
    schema match {
      case x: Switch[ALG, _, _, _] => {
        val tableName = camelToSnake(x.manifestOfA.runtimeClass.getSimpleName)
        val updateF =
          DbUpdate.valueDefinition(idDef.asSchema, customDbUpdateInterpreter)(1, idDef.key)
        val sql =
          s"delete from ${tableName} where ${updateF.assignmentStatements.map(_._1).mkString(" AND ")}"
        val getEntity = DbGet.getEntity(
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
      case _ => ??? // TODO
    }
  }

}
