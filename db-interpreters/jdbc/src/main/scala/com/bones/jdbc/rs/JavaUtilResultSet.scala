package com.bones.jdbc.rs

import java.sql.ResultSet

import com.bones.Path
import com.bones.Util.{CanBeOmitted, stringToUuid}
import com.bones.data.Error.ExtractionErrors
import com.bones.data.values.{JavaUtilValue, UuidData}
import com.bones.jdbc.FindInterpreter.FieldName

trait JavaUtilResultSet extends ResultSetValue[JavaUtilValue] {
  override def resultSet[A](alg: JavaUtilValue[A]): (
    Path[String],
    FieldName) => ResultSet => Either[ExtractionErrors[String], CanBeOmitted[String, A]] =
    alg match {
      case uu: UuidData =>
        (path, fieldName) => rs =>
          for {
            nullable <- catchSql[String](rs.getString(fieldName), uu.typeName, fieldName, path)
            uuid <- nullable match {
              case Right(value) =>
                stringToUuid(value, path).map(uuid => Right(uuid))
              case Left(nullable) => Right(Left(nullable))
            }
          } yield uuid
    }
}
