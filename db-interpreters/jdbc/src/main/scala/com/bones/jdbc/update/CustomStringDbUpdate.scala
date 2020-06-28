package com.bones.jdbc.update

import java.sql.Types

import com.bones.data.values.CustomStringValue
import com.bones.jdbc.update.DbUpdate.{Index, Key, psF}

trait CustomStringDbUpdate extends DbUpdateValue[CustomStringValue] {
  override def definitionResult[A](alg: CustomStringValue[A]): (Index, Key) => DbUpdate.DefinitionResult[A] =
    psF[A](i => (ps, a) => ps.setString(i, a.asInstanceOf[String]), Types.LONGVARCHAR)
}
