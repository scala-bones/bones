package com.bones.jdbc.rs

import java.sql.ResultSet

import com.bones.Util.NullableResult
import com.bones.data.Error.ExtractionErrors
import com.bones.data.values.CustomStringValue
import com.bones.jdbc.FindInterpreter.{FieldName, Path}

trait CustomStringResultSetValue extends ResultSetValue[CustomStringValue] {
  override def resultSet[A](alg: CustomStringValue[A])
    : (Path, FieldName) => ResultSet => Either[ExtractionErrors, NullableResult[A]] =
    (path, fieldName) =>
      rs => catchSql(rs.getString(fieldName).asInstanceOf[A], alg.typeName, fieldName, path)

}
