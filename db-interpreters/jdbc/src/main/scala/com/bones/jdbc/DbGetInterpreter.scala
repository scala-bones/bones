package com.bones.jdbc

import java.sql.Connection

import cats.data.NonEmptyList
import com.bones.data.Error.{ExtractionError, NotFound, SystemError}
import com.bones.data.KvpCollection.headManifest
import com.bones.data.{KvpCollection, KvpCollectionValue, PrimitiveWrapperValue}
import com.bones.jdbc.DbUtil.{camelToSnake, withStatement}
import com.bones.jdbc.column.ColumnNameInterpreter
import com.bones.jdbc.rs.ResultSetInterpreter
import com.bones.jdbc.update.{DbUpdate, JdbcStatementInterpreter, UpdateStatementValue}
import shapeless.HNil

import scala.util.control.NonFatal

trait DbGetInterpreter[ALG[_]] {

  def columnNameInterpreter: ColumnNameInterpreter[ALG]
  def jdbcStatementInterpreter: JdbcStatementInterpreter[ALG]
  def resultSetInterpreter: ResultSetInterpreter[ALG]

  /**
    * Creates a function which loads an entity by ID.
    * This function expects a connection, managed externally.
    * @param schema The description of the object being loaded.
    * @param idDefinition The description of the table's primary key column.
    * @tparam A The resulting type.
    * @tparam ID The type of the ID (eg, Int, Long, UUID)
    * @return A Curried Function which when given a Connection and an ID, will fetch the data from the DB.
    */
  def getEntity[A, ID](
    schema: KvpCollection[ALG, A],
    idDefinition: IdDefinition[ALG, ID],
  ): ID => Connection => Either[NonEmptyList[ExtractionError], (ID, A)] = {

    val entityName = headManifest(schema).map(_.runtimeClass.getSimpleName).getOrElse("unknown")
    val tableName = camelToSnake(entityName)
    val resultSetF =
      resultSetInterpreter
        .generateResultSet(schema)
        .apply(List.empty)

    val fields = columnNameInterpreter.generateColumnNames(schema)
    val idMeta =
      jdbcStatementInterpreter.fromKvpCollection(idDefinition.asSchema)(1)

    val sql =
      s"select ${fields.mkString(",")} from $tableName where id = ?"

    id =>
      { con =>
        {
          try {
            withStatement(con.prepareCall(sql))(statement => {
              idMeta.predicates(id :: HNil).foreach(_.apply(statement))
              val rs = statement.executeQuery()
              if (rs.next()) {
                val x = resultSetF(rs).map((id, _))
                x
              } else {
                Left(NonEmptyList.one(NotFound(id, entityName, List.empty)))
              }
            })
          } catch {
            case NonFatal(th) =>
              Left(NonEmptyList.one(SystemError(th, Some(s"SQL Statement: $sql"))))
          }
        }
      }
  }
}
