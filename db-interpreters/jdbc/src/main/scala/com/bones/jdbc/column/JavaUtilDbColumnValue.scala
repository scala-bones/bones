package com.bones.jdbc.column

import com.bones.data.values.{JavaUtilValue, UuidData}
import com.bones.jdbc.column.DbColumnInterpreter.{ToColumns, nameToColumn}

trait JavaUtilDbColumnValue extends ColumnValue[JavaUtilValue] {
  override def toColumns[A](alg: JavaUtilValue[A]): ToColumns =
    alg match {
      case uu: UuidData                    => nameToColumn("text")
    }
}
