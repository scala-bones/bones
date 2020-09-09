package com.bones.jdbc.insert

import com.bones.data.values.CustomStringValue

trait CustomStringDbInsert extends DbInsertValue[CustomStringValue] {
  override def insertPair[A](alg: CustomStringValue[A]): InsertPair[A] = ???
}
