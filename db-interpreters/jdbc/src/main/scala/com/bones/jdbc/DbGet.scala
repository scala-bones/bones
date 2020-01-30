package com.bones.jdbc

import java.sql.Connection

import cats.data.NonEmptyList
import com.bones.data.Error.{ExtractionError, NotFound, SystemError}
import com.bones.data.{BonesSchema, HListConvert}
import com.bones.jdbc.ColumnNameInterpreter.{CustomInterpreter => ColumnNameCustomInterpreter}
import com.bones.jdbc.ResultSetInterpreter.{CustomInterpreter => ResultSetCustomInterpreter}
import com.bones.jdbc.DbUtil.{camelToSnake, withStatement}
import com.bones.syntax.NoAlgebra
import javax.sql.DataSource

import scala.util.control.NonFatal
import java.sql.ResultSet

object DbGet {

  type DbGetCustomInterpreter[ALG[_]] = ColumnNameCustomInterpreter[ALG]
    with ResultSetCustomInterpreter[ALG]

  object NoAlgebraDbGetCustomInterpreter
      extends ColumnNameCustomInterpreter[NoAlgebra]
      with ResultSetCustomInterpreter[NoAlgebra] {
    def keyToColumnNames[A](
      alg: NoAlgebra[A]): ColumnNameInterpreter.Key => List[ColumnNameInterpreter.ColumnName] =
      sys.error("Unreachable code")
    def resultSet[A](
      alg: NoAlgebra[A]): (FindInterpreter.Path, FindInterpreter.FieldName) => ResultSet => Either[
      NonEmptyList[com.bones.data.Error.ExtractionError],
      A] =
      sys.error("Unreachable code")
  }

  def getEntity[ALG[_], A](
    schema: BonesSchema[ALG, A],
    columnInterpreter: DbGetCustomInterpreter[ALG]
  ): DataSource => Long => Either[NonEmptyList[ExtractionError], (Long, A)] = {
    val withConnection = getEntityWithConnectionCustomAlgebra(schema, columnInterpreter)
    ds =>
      { id =>
        {
          DbUtil.withDataSource(ds)(con => withConnection(id)(con))
        }
      }
  }

  def getEntityWithConnection[A](schema: BonesSchema[NoAlgebra, A])
    : Long => Connection => Either[NonEmptyList[ExtractionError], (Long, A)] =
    getEntityWithConnectionCustomAlgebra(schema, NoAlgebraDbGetCustomInterpreter)

  def getEntityWithConnectionCustomAlgebra[ALG[_], A](
    schema: BonesSchema[ALG, A],
    columnInterpreter: DbGetCustomInterpreter[ALG]
  ): Long => Connection => Either[NonEmptyList[ExtractionError], (Long, A)] = {
    schema match {
      case xMap: HListConvert[ALG, a, al, b] => {
        implicit val x = xMap.manifestOfA
        val schemaWithId =
          (DbUtil.longIdKeyValueDef[ALG] >>: schema :><: com.bones.syntax.kvpNilCov[ALG])
            .tupled[(Long, A)]

        id =>
          {
            val tableName = camelToSnake(xMap.manifestOfA.runtimeClass.getSimpleName)
            val resultSetF =
              ResultSetInterpreter.valueDefinition(schemaWithId, columnInterpreter)(List.empty, "")

            val fields = ColumnNameInterpreter.valueDefinition(schemaWithId, columnInterpreter)("")
            val sql =
              s"select ${fields.mkString(",")} from $tableName where id = ?"
            con =>
              {
                try {
                  withStatement(con.prepareCall(sql))(statement => {
                    statement.setLong(1, id)
                    val rs = statement.executeQuery()
                    if (rs.next()) {
                      resultSetF(rs)
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
