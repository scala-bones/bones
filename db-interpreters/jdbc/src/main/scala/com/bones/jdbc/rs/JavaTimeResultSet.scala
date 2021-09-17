package com.bones.jdbc.rs

import java.sql.ResultSet
import java.time.{LocalDateTime, ZoneId}
import java.util.Date

import com.bones.Path
import com.bones.Util.CanBeOmitted
import com.bones.data.Error.ExtractionErrors
import com.bones.data.values.{JavaTimeValue, LocalDateData, LocalDateTimeData}
import com.bones.jdbc.FindInterpreter.{FieldName, utcCalendar}

trait JavaTimeResultSet extends ResultSetValue[JavaTimeValue] {
  override def resultSet[A](alg: JavaTimeValue[A]): (
    Path[String],
    FieldName
  ) => ResultSet => Either[ExtractionErrors[String], CanBeOmitted[String, A]] =
    alg match {
      case ld: LocalDateData =>
        (path, fieldName) =>
          rs =>
            catchSql(rs.getDate(fieldName, utcCalendar), ld.typeName, fieldName, path)
              .map(date => date.map(_.toLocalDate))
      case dd: LocalDateTimeData =>
        (path, fieldName) =>
          rs =>
            catchSql(rs.getDate(fieldName, utcCalendar), dd.typeName, fieldName, path)
              .map(nullableDate => {
                nullableDate.map(date => {
                  LocalDateTime
                    .ofInstant(new Date(date.getTime).toInstant, ZoneId.of("UTC"))
                })
              })
      case _ => ??? // TODO

    }
}
