package com.bones.jdbc.column

import com.bones.data.values.{JavaTimeValue, LocalDateData, LocalDateTimeData}
import com.bones.jdbc.column.DbColumnInterpreter.{ColumnInterpreter, ToColumns, nameToColumn}

trait JavaTimeDbColumnInterpreter extends ColumnInterpreter[JavaTimeValue] {
  override def toColumns[A](alg: JavaTimeValue[A]): ToColumns =
    alg match {
      case dd: LocalDateData               => nameToColumn("date")
      case dd: LocalDateTimeData           => nameToColumn("timestamp")
    }
}
