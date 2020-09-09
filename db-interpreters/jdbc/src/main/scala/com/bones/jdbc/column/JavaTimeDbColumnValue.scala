package com.bones.jdbc.column

import com.bones.data.values.{JavaTimeValue, LocalDateData, LocalDateTimeData}

trait JavaTimeDbColumnValue extends ColumnValue[JavaTimeValue] {
  override def toColumns[A](alg: JavaTimeValue[A]): ToColumns =
    alg match {
      case _: LocalDateData     => nameToColumn("date")
      case _: LocalDateTimeData => nameToColumn("timestamp")
      case _                    => ??? // TODO
    }
}
