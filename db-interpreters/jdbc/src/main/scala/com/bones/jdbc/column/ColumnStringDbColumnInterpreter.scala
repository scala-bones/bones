package com.bones.jdbc.column

import com.bones.data.custom.CustomStringValue
import com.bones.jdbc.column.DbColumnInterpreter.{ColumnInterpreter, ToColumns, nameToColumn}

trait ColumnStringDbColumnInterpreter extends ColumnInterpreter[CustomStringValue] {
  override def toColumns[A](alg: CustomStringValue[A]): ToColumns =
    nameToColumn("text")
}
