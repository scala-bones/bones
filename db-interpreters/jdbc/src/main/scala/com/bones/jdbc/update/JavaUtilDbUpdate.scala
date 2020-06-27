package com.bones.jdbc.update

import java.sql.Types

import com.bones.data.values.{JavaUtilValue, UuidData}
import com.bones.jdbc.update.DbUpdateValues.{CustomDbUpdateInterpreter, Index, Key, psF}

trait JavaUtilDbUpdate extends CustomDbUpdateInterpreter[JavaUtilValue] {
  override def definitionResult[A](alg: JavaUtilValue[A]): (Index, Key) => DbUpdateValues.DefinitionResult[A] =
    alg match {
      case uu: UuidData =>
        psF(i => (ps, a) => ps.setString(i, a.toString), Types.VARCHAR)


    }
}
