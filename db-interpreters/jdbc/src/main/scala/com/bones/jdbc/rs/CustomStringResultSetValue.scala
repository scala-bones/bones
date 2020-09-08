package com.bones.jdbc.rs

import java.sql.ResultSet

import cats.data.NonEmptyList
import com.bones.data.Error
import com.bones.data.values.CustomStringValue
import com.bones.jdbc.FindInterpreter.{FieldName, Path}

trait CustomStringResultSetValue extends ResultSetValue[CustomStringValue] {
  override def resultSet[A](alg: CustomStringValue[A])
    : (Path, FieldName) => ResultSet => Either[NonEmptyList[Error.ExtractionError], A] =
    (path, fieldName) => rs => catchSql(rs.getString(fieldName).asInstanceOf[A], path, alg)

}
