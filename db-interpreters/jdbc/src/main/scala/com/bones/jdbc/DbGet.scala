package com.bones.jdbc

import java.sql.Connection

import cats.data.NonEmptyList
import com.bones.data.Error.{ExtractionError, NotFound, SystemError}
import com.bones.data.{HListConvert, KvpCollection}
import com.bones.jdbc.DbUtil.{camelToSnake, withStatement}
import com.bones.jdbc.column.ColumnNameInterpreter
import com.bones.jdbc.rs.{ResultSetInterpreter, ResultSetValue => ResultSetCustomInterpreter}
import com.bones.jdbc.update.{DbUpdateValue, DbUpdate}
import javax.sql.DataSource

import scala.util.control.NonFatal

object DbGet {

  def getEntity[ALG[_], A, ID](
    schema: KvpCollection[ALG, A],
    idDefinition: IdDefinition[ALG, ID],
    resultSetCustomInterpreter: ResultSetCustomInterpreter[ALG],
    customDbUpdateInterpreter: DbUpdateValue[ALG]
  ): DataSource => ID => Either[NonEmptyList[ExtractionError], (ID, A)] = {
    val withConnection = getEntityWithConnectionCustomAlgebra(
      schema,
      idDefinition,
      resultSetCustomInterpreter,
      customDbUpdateInterpreter)
    ds =>
      { id =>
        DbUtil.withDataSource(ds)(con => withConnection(id)(con))
      }
  }

  def getEntityWithConnectionCustomAlgebra[ALG[_], A, ID](
    schema: KvpCollection[ALG, A],
    idDefinition: IdDefinition[ALG, ID],
    resultSetCustomInterpreter: ResultSetCustomInterpreter[ALG],
    customDbUpdateInterpreter: DbUpdateValue[ALG]
  ): ID => Connection => Either[NonEmptyList[ExtractionError], (ID, A)] = {
    schema match {
      case xMap: HListConvert[ALG, a, al, b] => {
        val schemaWithId = idDefinition.prependSchema(schema)
        id =>
          {
            val tableName = camelToSnake(xMap.manifestOfA.runtimeClass.getSimpleName)
            val resultSetF =
              ResultSetInterpreter
                .generateResultSet(schema, resultSetCustomInterpreter)
                .apply(List.empty)

            val fields = ColumnNameInterpreter.generateColumnNames(schema)
            val idMeta =
              DbUpdate.determineValueDefinition(
                Right(idDefinition.value),
                customDbUpdateInterpreter)(1, "id")

            val sql =
              s"select ${fields.mkString(",")} from $tableName where id = ?"
            con =>
              {
                try {
                  withStatement(con.prepareCall(sql))(statement => {
                    idMeta.predicates(id).foreach(_.apply(statement))
                    val rs = statement.executeQuery()
                    if (rs.next()) {
                      val x = resultSetF(rs).map((id, _))
                      x
                    } else {
                      Left(
                        NonEmptyList.one(
                          NotFound(id, xMap.manifestOfA.runtimeClass.getSimpleName, List.empty)))
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
  }
}
