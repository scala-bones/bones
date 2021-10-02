package com.bones

import java.util.UUID

import com.bones.data.values.DefaultValues
import com.bones.data.{KvpCollection, KvpNil, KvpSingleValueHead, KvpWrappedHList}
import com.bones.jdbc.column.{ColumnNameInterpreter, ColumnValue}
import com.bones.jdbc.rs.ResultSetInterpreter
import com.bones.jdbc.select.SelectInterpreter
import com.bones.jdbc.update.{JdbcStatementInterpreter, UpdateStatementValue}
import com.bones.syntax.{int, iv, kvpNil, long, lv, uuid, uuidV}
import com.bones.validation.ValidationDefinition.{UniqueValue, ValidationOp}
import shapeless.Nat._0
import shapeless.{::, HNil, Succ}

package object jdbc {

  val longId: KvpSingleValueHead[String, DefaultValues, Long, HNil, _0, Long :: HNil] =
    ("id", long(lv.min(0), lv.unique)) :: kvpNil
  val intId: KvpSingleValueHead[String, DefaultValues, Int, HNil, _0, Int :: HNil] =
    ("id", int(iv.min(0), iv.unique)) :: kvpNil
  val uuidId: KvpSingleValueHead[String, DefaultValues, UUID, HNil, _0, UUID :: HNil] =
    ("id", uuid(uuidV.unique)) :: kvpNil

  case class JdbcColumnInterpreter[ALG[_]](
    resultSet: rs.ResultSetInterpreter[ALG],
    dbColumn: ColumnValue[ALG],
    insert: com.bones.jdbc.insert.DbInsert[ALG],
    dbUpdate: UpdateStatementValue[ALG]
  )

  val defaultJdbcColumnInterpreter =
    JdbcColumnInterpreter(
      com.bones.jdbc.rs.defaultResultSetInterpreter,
      com.bones.jdbc.column.defaultDbColumnValues,
      com.bones.jdbc.insert.defaultDbInsertInterpreter,
      com.bones.jdbc.update.defaultUpdateStatementInterpreter
    )

  case class IdDefinition[ALG[_], ID: Manifest](key: String, value: ALG[ID]) {

    def asTuple: (String, ALG[ID]) = (key, value)

    def prependSchema[A: Manifest](
      schema: KvpCollection[String, ALG, A]
    ): KvpWrappedHList[String, ALG, (ID, A), ID :: A :: HNil, Succ[Succ[_0]]] = {
      (asTuple :: schema :: KvpNil[String, ALG]).tupled[(ID, A)]
    }

    def asSchema: KvpCollection[String, ALG, ID :: HNil] = {
      asTuple :: new KvpNil[String, ALG]
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
    v.collect { case uv: UniqueValue[_] =>
      uv
    }

}
