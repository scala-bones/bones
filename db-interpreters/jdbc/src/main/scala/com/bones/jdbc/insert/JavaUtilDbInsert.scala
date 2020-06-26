package com.bones.jdbc.insert

import java.util.UUID

import com.bones.data.custom.{JavaUtilValue, UuidData}
import com.bones.jdbc.insert.DbInsertValues.{InsertPair, psF}

trait JavaUtilDbInsert extends CustomInterpreter[JavaUtilValue] {
  override def insertPair[A](alg: JavaUtilValue[A]): InsertPair[A] =
    alg match {
      case uu: UuidData =>
        psF[UUID]((ps, i, a) => ps.setString(i, a.toString))
    }
}
