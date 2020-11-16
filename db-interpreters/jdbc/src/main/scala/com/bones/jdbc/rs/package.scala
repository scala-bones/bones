package com.bones.jdbc

import java.sql.SQLException

import com.bones.Path
import com.bones.Util.{NullValue, NullableResult}
import com.bones.data.Error.{ExtractionErrors, SystemError}
import com.bones.data.values.DefaultValues

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
    path: Path[String]): Either[ExtractionErrors[String], NullableResult[String, A]] =
    try {
      val result = f
      if (result == null) {
        Right(Left(List(NullValue(fieldName, typeName, path))))
      } else {
        Right(Right(result))
      }
    } catch {
      case ex: SQLException =>
        Left(List(SystemError(path, ex, Some(s"Error extracting field: ${fieldName}"))))
    }

}
