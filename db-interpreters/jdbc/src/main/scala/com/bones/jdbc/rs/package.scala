package com.bones.jdbc

import java.sql.ResultSet

import cats.data.NonEmptyList
import com.bones.data.Error.ExtractionError
import com.bones.data.values.{DefaultValues, CNilF}
import com.bones.jdbc.FindInterpreter.{FieldName, Path}
import shapeless.{:+:, Coproduct, Inl, Inr}

package object rs {

  val defaultResultSetInterpreter: ResultSetValue[DefaultValues] =
    (DefaultScalaCoreResultSet ++
      (DefaultCustomStringResultSet ++
        (DefaultJavaTimeResultSet ++
          (DefaultJavaUtilResultSet ++ ResultSetValue.CNilResultSetValue))))

  object DefaultJavaTimeResultSet extends JavaTimeResultSet
  object DefaultJavaUtilResultSet extends JavaUtilResultSet
  object DefaultCustomStringResultSet extends CustomStringResultSetValue
  object DefaultScalaCoreResultSet extends ScalaCoreResultSet



}
