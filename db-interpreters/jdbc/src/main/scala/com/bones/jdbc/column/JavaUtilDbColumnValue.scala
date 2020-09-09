package com.bones.jdbc.column

import com.bones.data.values.{JavaUtilValue, UuidData}

trait JavaUtilDbColumnValue extends ColumnValue[JavaUtilValue] {
  override def toColumns[A](alg: JavaUtilValue[A]): ToColumns =
    alg match {
      case uu: UuidData => nameToColumn("text")
    }
}
