package com.bones.jdbc.insert

import java.util.UUID

import com.bones.data.values.{JavaUtilValue, UuidData}
import com.bones.jdbc.insert.DbInsert.{InsertPair, psF}

trait JavaUtilDbInsert extends DbInsertValue[JavaUtilValue] {
  override def insertPair[A](alg: JavaUtilValue[A]): InsertPair[A] =
    alg match {
      case uu: UuidData =>
        psF[UUID]((ps, i, a) => ps.setString(i, a.toString))
    }
}
