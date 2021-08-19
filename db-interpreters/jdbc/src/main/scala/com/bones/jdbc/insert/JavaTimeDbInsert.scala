package com.bones.jdbc.insert

import java.sql.Timestamp
import java.time.{LocalDate, LocalDateTime, ZoneOffset}

import com.bones.data.values.{JavaTimeValue, LocalDateData, LocalDateTimeData}

trait JavaTimeDbInsert extends DbInsertValue[JavaTimeValue] {
  override def insertPair[A](alg: JavaTimeValue[A]): InsertPair[A] =
    alg match {
      case dd: LocalDateTimeData =>
        psF[LocalDateTime]((ps, i, a) =>
          ps.setTimestamp(i, new Timestamp(a.toInstant(ZoneOffset.UTC).toEpochMilli))
        )
      case ld: LocalDateData =>
        psF[LocalDate]((ps, i, a) =>
          ps.setDate(i, new java.sql.Date(a.atStartOfDay().toEpochSecond(ZoneOffset.UTC)))
        )
      case _ => ??? // TODO
    }
}
