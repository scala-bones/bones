package com.bones.jdbc.rs

import java.sql.ResultSet

import cats.data.NonEmptyList
import com.bones.Util.stringToUuid
import com.bones.data.Error
import com.bones.data.values.{JavaUtilValue, UuidData}
import com.bones.jdbc.FindInterpreter.{FieldName, Path}
import com.bones.jdbc.rs.ResultSetInterpreter.catchSql

trait JavaUtilResultSet extends ResultSetValue[JavaUtilValue] {
  override def resultSet[A](alg: JavaUtilValue[A]): (Path, FieldName) => ResultSet => Either[NonEmptyList[Error.ExtractionError], A] =
    alg match {
      case uu: UuidData =>
        (path, fieldName) => rs =>
          catchSql[String](rs.getString(fieldName), path, uu)
            .flatMap(str => stringToUuid(str, path))

    }
}
