package com.bones.jdbc.update

import java.sql.{PreparedStatement, Types}
import java.time.{LocalDate, LocalDateTime, ZoneOffset}

import com.bones.data.values.{JavaTimeValue, LocalDateData, LocalDateTimeData}

trait JavaTimeDbUpdate extends DbUpdateValue[JavaTimeValue] {
  import DbUpdate._
  override def definitionResult[A](alg: JavaTimeValue[A]): (Index, Key) => DbUpdate.DefinitionResult[A] =
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
      case _ => ??? // TODO
    }
}
