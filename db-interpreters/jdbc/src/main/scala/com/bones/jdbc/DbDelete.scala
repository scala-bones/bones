package com.bones.jdbc

import java.sql.Connection

import cats.data.NonEmptyList
import com.bones.data.Error.{ExtractionErrors, SystemError}
import com.bones.data.KvpCollection
import com.bones.data.KvpCollection.headTypeName
import com.bones.jdbc.DbUtil.{camelToSnake, withStatement}
import com.bones.jdbc.select.SelectInterpreter
import com.bones.jdbc.update.JdbcStatementInterpreter

import scala.util.control.NonFatal

trait DbDelete[ALG[_]] {

  def dbGet: SelectInterpreter[ALG]
  def jdbcStatementInterpreter: JdbcStatementInterpreter[ALG]

  /**
    * Creates a curried function which takes an ID and a connection
    * and then deletes the entity in the database with the specified id.
    * @param schema
    * @param idSchema
    * @tparam A
    * @tparam ID
    * @return
    */
  def delete[A, ID](
    schema: KvpCollection[String, ALG, A],
    idSchema: KvpCollection[String, ALG, ID],
  ): ID => Connection => Either[ExtractionErrors[String], ID] = {
    val tableName = camelToSnake(headTypeName(schema).getOrElse("Unknown"))
    val updateF =
      jdbcStatementInterpreter.fromKvpCollection(idSchema)(1)
    val sql =
      s"delete from ${tableName} where ${updateF.assignmentStatements.map(_._1).mkString(" AND ")}"
    val getEntity =
      dbGet.selectEntity(schema, idSchema)
    id => con =>
      {
        try {
          for {
            _ <- {
              withStatement[Boolean](con.prepareCall(sql))(statement => {
                updateF.predicates(id).foreach(_.apply(statement))
                Right(statement.execute())
              })
            }
          } yield id
        } catch {
          case NonFatal(ex) =>
            Left(List(SystemError(ex, Some("Error deleting entity"))))
        }
      }
  }

}
