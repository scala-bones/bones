package com.bones.jdbc.update

import java.sql.Types

import com.bones.data.values.{JavaUtilValue, UuidData}

trait JavaUtilUpdateStatement extends UpdateStatementValue[JavaUtilValue] {
  override def definitionResult[A](alg: JavaUtilValue[A]): (Index, Key) => JdbcColumnStatement[A] =
    alg match {
      case uu: UuidData =>
        psF(i => (ps, a) => ps.setString(i, a.toString), Types.VARCHAR)

    }
}
