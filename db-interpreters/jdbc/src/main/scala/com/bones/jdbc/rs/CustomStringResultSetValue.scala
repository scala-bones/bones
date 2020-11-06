package com.bones.jdbc.rs

import java.sql.ResultSet

import com.bones.Path
import com.bones.Util.NullableResult
import com.bones.data.Error.ExtractionErrors
import com.bones.data.values.CustomStringValue
import com.bones.jdbc.FindInterpreter.FieldName

trait CustomStringResultSetValue extends ResultSetValue[CustomStringValue] {
  override def resultSet[A](alg: CustomStringValue[A]): (
    Path[String],
    FieldName) => ResultSet => Either[ExtractionErrors[String], NullableResult[String, A]] =
    (path, fieldName) =>
      rs => catchSql(rs.getString(fieldName).asInstanceOf[A], alg.typeName, fieldName, path)

}
