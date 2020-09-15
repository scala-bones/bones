package com.bones.jdbc

import java.sql.SQLException

import cats.data.NonEmptyList
import com.bones.PrimitiveValue
import com.bones.data.Error.{ExtractionError, RequiredValue, SystemError}
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
    path: Path,
    op: PrimitiveValue[_]): Either[NonEmptyList[ExtractionError], A] =
    try {
      val result = f
      if (result == null) {
        Left(NonEmptyList.one(RequiredValue(path, typeName)))
      } else {
        Right(result)
      }
    } catch {
      case ex: SQLException =>
        Left(NonEmptyList.one(SystemError(path, ex, None)))
    }

}
