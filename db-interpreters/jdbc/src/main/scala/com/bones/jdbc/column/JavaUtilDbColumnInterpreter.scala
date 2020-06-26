package com.bones.jdbc.column

import com.bones.data.custom.{JavaUtilValue, UuidData}
import com.bones.jdbc.column.DbColumnInterpreter.{ColumnInterpreter, ToColumns, nameToColumn}

trait JavaUtilDbColumnInterpreter extends ColumnInterpreter[JavaUtilValue] {
  override def toColumns[A](alg: JavaUtilValue[A]): ToColumns =
    alg match {
      case uu: UuidData                    => nameToColumn("text")
    }
}
