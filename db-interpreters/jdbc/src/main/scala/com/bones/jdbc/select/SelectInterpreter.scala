package com.bones.jdbc.select

import java.sql.Connection

import com.bones.data.Error.{ExtractionErrors, NotFound, SystemError}
import com.bones.data.KvpCollection
import com.bones.data.KvpCollection.headTypeName
import com.bones.jdbc.DbUtil.{camelToSnake, withStatement}
import com.bones.jdbc.column.ColumnNameInterpreter
import com.bones.jdbc.rs.ResultSetInterpreter
import com.bones.jdbc.update.JdbcStatementInterpreter

import scala.util.control.NonFatal

trait SelectInterpreter[ALG[_]] {

  def columnNameInterpreter: ColumnNameInterpreter[ALG]
  def jdbcStatementInterpreter: JdbcStatementInterpreter[ALG]
  def resultSetInterpreter: ResultSetInterpreter[ALG]

  /** Creates a function which loads an entity by ID. This function expects a connection, managed
    * externally.
    * @param schema
    *   The description of the object being loaded.
    * @param idSchema
    *   The description of the table's primary key column.
    * @tparam A
    *   The resulting type.
    * @tparam ID
    *   The type of the ID (eg, Int, Long, UUID)
    * @return
    *   A Curried Function which when given a Connection and an ID, will fetch the data from the DB.
    */
  def selectEntity[A, ID](
    schema: KvpCollection[String, ALG, A],
    idSchema: KvpCollection[String, ALG, ID],
    tableNameOverride: Option[String] = None
  ): ID => Connection => Either[ExtractionErrors[String], (ID, A)] = {

    val entityName = headTypeName(schema).getOrElse("Unknown")
    val tableName = tableNameOverride.getOrElse(camelToSnake(entityName))
    val resultSetF =
      resultSetInterpreter
        .generateResultSet(schema)
        .apply(List.empty)

    val fields = columnNameInterpreter.generateColumnNames(schema)
    val idMeta =
      jdbcStatementInterpreter.fromKvpCollection(idSchema)(1)

    val sql =
      s"select ${fields.mkString(",")} from $tableName where id = ?"

    id => { con =>
      {
        try {
          withStatement(con.prepareCall(sql))(statement => {
            idMeta.predicates(id).foreach(_.apply(statement))
            val rs = statement.executeQuery()
            if (rs.next()) {
              val x = resultSetF(rs).map((id, _))
              x
            } else {
              Left(List(NotFound(id, entityName, List.empty)))
            }
          })
        } catch {
          case NonFatal(th) =>
            Left(List(SystemError(th, Some(s"SQL Statement: $sql"))))
        }
      }
    }
  }
}
