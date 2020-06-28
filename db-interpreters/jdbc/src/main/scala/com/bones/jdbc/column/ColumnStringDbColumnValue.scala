package com.bones.jdbc.column

import com.bones.data.values.CustomStringValue
import com.bones.jdbc.column.DbColumnInterpreter.{ToColumns, nameToColumn}

trait ColumnStringDbColumnValue extends ColumnValue[CustomStringValue] {
  override def toColumns[A](alg: CustomStringValue[A]): ToColumns =
    nameToColumn("text")
}
