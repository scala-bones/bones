package com.bones.jdbc

import java.sql.Connection

import cats.data.NonEmptyList
import com.bones.data.Error.{ExtractionError, SystemError}
import com.bones.data.KvpCollection.headManifest
import com.bones.data.{KvpCollection, KvpCollectionValue, HigherOrderValue}
import com.bones.jdbc.DbUtil.{camelToSnake, withStatement}
import com.bones.jdbc.rs.{ResultSetInterpreter, ResultSetValue}
import com.bones.jdbc.update.{DbUpdate, JdbcStatementInterpreter, UpdateStatementValue}
import shapeless.HNil

import scala.util.control.NonFatal

trait DbDelete[ALG[_]] {

  def dbGet: DbGetInterpreter[ALG]
  def jdbcStatementInterpreter: JdbcStatementInterpreter[ALG]

  /**
    * Creates a curried function which takes an ID and a connection
    * and then deletes the entity in the database with the specified id.
    * @param schema
    * @param idDef
    * @tparam A
    * @tparam ID
    * @return
    */
  def delete[A, ID](
    schema: KvpCollection[ALG, A],
    idDef: IdDefinition[ALG, ID],
  ): ID => Connection => Either[NonEmptyList[ExtractionError], (ID, A)] = {
    val tableName = camelToSnake(
      headManifest(schema).map(_.runtimeClass.getSimpleName).getOrElse("unknown"))
    val updateF =
      jdbcStatementInterpreter.fromKvpCollection(idDef.asSchema)(1)
    val sql =
      s"delete from ${tableName} where ${updateF.assignmentStatements.map(_._1).mkString(" AND ")}"
    val getEntity =
      dbGet.getEntity(schema, idDef)
    id => con =>
      {
        try {
          for {
            entity <- getEntity(id)(con)
            _ <- {
              withStatement[Boolean](con.prepareCall(sql))(statement => {
                updateF.predicates(id :: HNil).foreach(_.apply(statement))
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
