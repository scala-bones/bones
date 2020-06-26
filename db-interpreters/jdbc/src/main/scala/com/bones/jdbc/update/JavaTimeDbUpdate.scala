package com.bones.jdbc.update

import java.sql.{PreparedStatement, Types}
import java.time.{LocalDate, LocalDateTime, ZoneOffset}

import com.bones.data.custom.{JavaTimeValue, LocalDateData, LocalDateTimeData}
import com.bones.jdbc.update.DbUpdateValues.{CustomDbUpdateInterpreter, Index, Key}

trait JavaTimeDbUpdate extends CustomDbUpdateInterpreter[JavaTimeValue] {
  import DbUpdateValues._
  override def definitionResult[A](alg: JavaTimeValue[A]): (Index, Key) => DbUpdateValues.DefinitionResult[A] =
    alg match {
      case dd: LocalDateTimeData =>
        psF(
          (i: Index) =>
            (ps: PreparedStatement, a: LocalDateTime) =>
              ps.setDate(i, new java.sql.Date(a.toInstant(ZoneOffset.UTC).toEpochMilli)),
          Types.DATE)
      case ld: LocalDateData =>
        psF(
          (i: Index) =>
            (ps: PreparedStatement, a: LocalDate) =>
              ps.setDate(
                i,
                new java.sql.Date(a.atStartOfDay.toInstant(ZoneOffset.UTC).toEpochMilli)),
          Types.DATE)
    }
}
