package com.bones.jdbc.update

import java.sql.Types

import com.bones.data.values.CustomStringValue

trait CustomStringUpdateStatement extends UpdateStatementValue[CustomStringValue] {
  override def definitionResult[A](
    alg: CustomStringValue[A]): (Index, Key) => JdbcColumnStatement[A] =
    psF[A](i => (ps, a) => ps.setString(i, a.asInstanceOf[String]), Types.LONGVARCHAR)
}
