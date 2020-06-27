package com.bones.jdbc.insert

import com.bones.data.values.CustomStringValue
import com.bones.jdbc.insert.DbInsertValues.InsertPair

trait CustomStringDbInsert extends CustomInterpreter[CustomStringValue] {
  override def insertPair[A](alg: CustomStringValue[A]): InsertPair[A] = ???
}
