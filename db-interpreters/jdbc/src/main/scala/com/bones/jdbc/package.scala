package com.bones

import com.bones.data.{SwitchEncoding, ConcreteValue, KvpNil}
import com.bones.jdbc.column.ColumnValue
import com.bones.jdbc.update.DbUpdateValue
import shapeless.Nat._0
import shapeless.{::, HNil, Succ}

package object jdbc {

  case class JdbcColumnInterpreter[ALG[_]](
    resultSet: rs.ResultSetValue[ALG],
    dbColumn: ColumnValue[ALG],
    insert: com.bones.jdbc.insert.DbInsertValue[ALG],
    dbUpdate: DbUpdateValue[ALG])

  val defaultJdbcColumnInterpreter =
    JdbcColumnInterpreter(
      com.bones.jdbc.rs.defaultResultSetInterpreter,
      com.bones.jdbc.column.defaultDbColumnInterpreter,
      com.bones.jdbc.insert.defaultDbInsertInterpreter,
      com.bones.jdbc.update.defaultDbUpdateInterpreter
    )

  case class IdDefinition[ALG[_], ID: Manifest](key: String, value: ALG[ID]) {

    def asTuple: (String, ALG[ID]) = (key, value)

    def prependSchema[A](schema: ConcreteValue[ALG, A]) = {
      implicit val manifest = schema.manifestOfA
      (asTuple :: schema :><: new KvpNil[ALG]).tupled[(ID, A)]
    }

    def asSchema: SwitchEncoding[ALG, ID :: HNil, Succ[_0], ID] = {
      val base = asTuple :: new KvpNil[ALG]
      SwitchEncoding.apply(base, _.head, _ :: HNil, List.empty)
    }

  }

}
