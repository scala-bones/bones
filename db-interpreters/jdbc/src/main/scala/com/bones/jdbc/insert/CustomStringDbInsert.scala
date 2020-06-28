package com.bones.jdbc.insert

import com.bones.data.values.CustomStringValue
import com.bones.jdbc.insert.DbInsert.InsertPair

trait CustomStringDbInsert extends DbInsertValue[CustomStringValue] {
  override def insertPair[A](alg: CustomStringValue[A]): InsertPair[A] = ???
}
