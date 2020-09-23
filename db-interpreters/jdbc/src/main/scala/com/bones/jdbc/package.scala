package com.bones

import com.bones.data.KvpCollection.headTypeName
import com.bones.data.values.DefaultValues
import com.bones.data.{KvpCollection, KvpNil, KvpWrappedHList}
import com.bones.jdbc.column.{ColumnNameInterpreter, ColumnValue}
import com.bones.jdbc.rs.ResultSetInterpreter
import com.bones.jdbc.select.SelectInterpreter
import com.bones.jdbc.update.{JdbcStatementInterpreter, UpdateStatementValue}
import com.bones.validation.ValidationDefinition.{UniqueValue, ValidationOp}
import shapeless.Nat._0
import shapeless.{::, HNil, Succ}

package object jdbc {

  case class JdbcColumnInterpreter[ALG[_]](
    resultSet: rs.ResultSetInterpreter[ALG],
    dbColumn: ColumnValue[ALG],
    insert: com.bones.jdbc.insert.DbInsert[ALG],
    dbUpdate: UpdateStatementValue[ALG])

  val defaultJdbcColumnInterpreter =
    JdbcColumnInterpreter(
      com.bones.jdbc.rs.defaultResultSetInterpreter,
      com.bones.jdbc.column.defaultDbColumnValues,
      com.bones.jdbc.insert.defaultDbInsertInterpreter,
      com.bones.jdbc.update.defaultUpdateStatementInterpreter
    )

  case class IdDefinition[ALG[_], ID: Manifest](key: String, value: ALG[ID]) {

    def asTuple: (String, ALG[ID]) = (key, value)

    def prependSchema[A: Manifest](schema: KvpCollection[ALG, A])
      : KvpWrappedHList[ALG, (ID, A), ID :: A :: HNil, Succ[Succ[_0]]] = {
      (asTuple :: schema :: new KvpNil[ALG]).tupled[(ID, A)]
    }

    def asSchema: KvpCollection[ALG, ID :: HNil] = {
      asTuple :: new KvpNil[ALG]
    }

  }

  val dbSearchInterpreter = new DbSearch[DefaultValues] {
    override def resultSetInterpreter: ResultSetInterpreter[DefaultValues] =
      com.bones.jdbc.rs.defaultResultSetInterpreter

    override def columnNameInterpreter: ColumnNameInterpreter[DefaultValues] =
      com.bones.jdbc.column.defaultColumnNameInterpreter
  }

  val dbDeleteInterpreter = new DbDelete[DefaultValues] {
    override def dbGet: SelectInterpreter[DefaultValues] =
      select.defaultSelectInterpreter

    override def jdbcStatementInterpreter: JdbcStatementInterpreter[DefaultValues] =
      com.bones.jdbc.update.defaultJdbcStatementInterpreter
  }

  def findUniqueConstraint(v: List[ValidationOp[_]]): List[UniqueValue[_]] =
    v.flatMap {
      case uv: UniqueValue[_] => Some(uv)
      case _                  => None
    }

}
