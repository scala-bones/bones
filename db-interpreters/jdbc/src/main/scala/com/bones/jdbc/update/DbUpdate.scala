package com.bones.jdbc.update

import java.sql.{Connection, SQLException}

import cats.data.NonEmptyList
import com.bones.data.Error.SystemError
import com.bones.data.KvpCollection.headTypeName
import com.bones.data._
import com.bones.jdbc.DbUtil._

/** insert into table (field1, field2, field3) values (:value1, :value2, :value3) */
trait DbUpdate[ALG[_]] {

  def jdbcStatementInterpreter: JdbcStatementInterpreter[ALG]

  def updateQuery[A, ID](
    bonesSchema: KvpCollection[String, ALG, A],
    idSchema: KvpCollection[String, ALG, ID])
    : (ID, A) => Connection => Either[NonEmptyList[SystemError[String]], ID] = {
    val tableName = camelToSnake(headTypeName(bonesSchema).getOrElse("Unknown"))
    val updates = jdbcStatementInterpreter.fromKvpCollection(bonesSchema)(1)
    val idIndex = updates.lastIndex
    val idUpdateFunction =
      jdbcStatementInterpreter.fromKvpCollection(idSchema)(idIndex)
    // TODO this does not handle null/none case
    val sql =
      s"""update ${tableName} set ${updates.assignmentStatements
        .map(_._1)
        .mkString(",")} where ${idUpdateFunction.assignmentStatements
        .map(_._1)
        .mkString(" and ")}"""
    (id: ID, a: A) =>
      { (con: Connection) =>
        {
          val statement = con.prepareCall(sql)
          try {
            updates
              .predicates(a)
              .foreach(f => f(statement))
            idUpdateFunction.predicates(id).foreach(f => f.apply(statement))
            statement.execute()
            Right(id)
          } catch {
            case e: SQLException =>
              Left(NonEmptyList.one(SystemError(e, Some(sql))))
          } finally {
            statement.close()
          }
        }
      }
  }

}
