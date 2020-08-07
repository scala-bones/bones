package com.bones.jdbc

import java.sql.Connection

import cats.data.NonEmptyList
import com.bones.data.Error.{ExtractionError, NotFound, SystemError}
import com.bones.data.{SwitchEncoding, ConcreteValue}
import com.bones.jdbc.DbUtil.{camelToSnake, withStatement}
import com.bones.jdbc.column.ColumnNameInterpreter
import com.bones.jdbc.rs.{ResultSetInterpreter, ResultSetValue => ResultSetCustomInterpreter}
import com.bones.jdbc.update.{DbUpdate, DbUpdateValue}

import scala.util.control.NonFatal

object DbGet {

  /**
    * Creates a function which loads an entity by ID.
    * This function expects a connection, managed externally.
    * @param schema The description of the object being loaded.
    * @param idDefinition The description of the table's primary key column.
    * @param resultSetCustomInterpreter Used to convert the results set into the resulting type A
    * @param customDbUpdateInterpreter
    * @tparam ALG The algebra being used.
    * @tparam A The resulting type.
    * @tparam ID The type of the ID (eg, Int, Long, UUID)
    * @return A Curried Function which when given a Connection and an ID, will fetch the data from the DB.
    */
  def getEntity[ALG[_], A, ID](
    schema: ConcreteValue[ALG, A],
    idDefinition: IdDefinition[ALG, ID],
    resultSetCustomInterpreter: ResultSetCustomInterpreter[ALG],
    customDbUpdateInterpreter: DbUpdateValue[ALG]
  ): ID => Connection => Either[NonEmptyList[ExtractionError], (ID, A)] = {
    schema match {
      case sw: SwitchEncoding[ALG, a, al, b] => { id =>
        {
          val tableName = camelToSnake(sw.manifestOfA.runtimeClass.getSimpleName)
          val resultSetF =
            ResultSetInterpreter
              .generateResultSet(schema, resultSetCustomInterpreter)
              .apply(List.empty)

          val fields = ColumnNameInterpreter.generateColumnNames(schema)
          val idMeta =
            DbUpdate.determineValueDefinition(Right(idDefinition.value), customDbUpdateInterpreter)(
              1,
              "id")

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
                        NotFound(id, sw.manifestOfA.runtimeClass.getSimpleName, List.empty)))
                  }
                })
              } catch {
                case NonFatal(th) =>
                  Left(NonEmptyList.one(SystemError(th, Some(s"SQL Statement: $sql"))))
              }
            }
        }
      }
      case _ => ??? // TODO
    }
  }
}
