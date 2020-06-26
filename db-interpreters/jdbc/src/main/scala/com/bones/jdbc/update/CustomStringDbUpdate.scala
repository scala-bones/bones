package com.bones.jdbc.update

import java.sql.Types

import com.bones.data.custom.CustomStringValue
import com.bones.jdbc.update.DbUpdateValues.{CustomDbUpdateInterpreter, Index, Key, psF}

trait CustomStringDbUpdate extends CustomDbUpdateInterpreter[CustomStringValue] {
  override def definitionResult[A](alg: CustomStringValue[A]): (Index, Key) => DbUpdateValues.DefinitionResult[A] =
    psF[A](i => (ps, a) => ps.setString(i, a.asInstanceOf[String]), Types.LONGVARCHAR)
}
