package com.bones.jdbc

import java.sql.SQLException

import cats.data.NonEmptyList
import com.bones.PrimitiveValue
import com.bones.Util.{NullValue, NullableResult}
import com.bones.data.Error.{ExtractionError, ExtractionErrors, RequiredValue, SystemError}
import com.bones.data.values.DefaultValues
import com.bones.jdbc.FindInterpreter.Path

package object rs {

  val defaultResultSetValues: ResultSetValue[DefaultValues] =
    (DefaultScalaCoreResultSet ++
      (DefaultCustomStringResultSet ++
        (DefaultJavaTimeResultSet ++
          (DefaultJavaUtilResultSet ++ ResultSetValue.CNilResultSetValue))))

  object DefaultJavaTimeResultSet extends JavaTimeResultSet
  object DefaultJavaUtilResultSet extends JavaUtilResultSet
  object DefaultCustomStringResultSet extends CustomStringResultSetValue
  object DefaultScalaCoreResultSet extends ScalaCoreResultSet

  val defaultResultSetInterpreter = new ResultSetInterpreter[DefaultValues] {
    override val customInterpreter: ResultSetValue[DefaultValues] =
      defaultResultSetValues
  }

  def catchSql[A](
    f: => A,
    typeName: String,
    fieldName: String,
    path: Path): Either[ExtractionErrors, NullableResult[A]] =
    try {
      val result = f
      if (result == null) {
        Right(Left(NonEmptyList.one(NullValue(fieldName, typeName, path))))
      } else {
        Right(Right(result))
      }
    } catch {
      case ex: SQLException =>
        Left(NonEmptyList.one(SystemError(path, ex, Some(s"Error extracting field: ${fieldName}"))))
    }

}
