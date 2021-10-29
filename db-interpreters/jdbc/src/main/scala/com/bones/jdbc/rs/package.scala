package com.bones.jdbc

import java.sql.SQLException

import com.bones.Path
import com.bones.Util.{OmittedValue, CanBeOmitted}
import com.bones.data.Error.{ExtractionErrors, SystemError}
import com.bones.data.values.{
  CNilF,
  CustomStringValue,
  DefaultValues,
  JavaTimeValue,
  JavaUtilValue,
  ScalaCoreValue
}
import shapeless.:+:

package object rs {

//  val defaultResultSetValues: ResultSetValue[DefaultValues] =
//    (DefaultScalaCoreResultSet ++
//      (DefaultCustomStringResultSet ++
//        (DefaultJavaTimeResultSet ++
//          (DefaultJavaUtilResultSet ++ ResultSetValue.CNilResultSetValue))))

  // Below is equivalent to the above.  Above compiles in 2.13, below compiles in both 2.12 and 2.13
  // start 2.12

  type JavaUtilValueCo[A] = JavaUtilValue[A] :+: CNilF[A]
  type JavaTimeValueCo[A] = JavaTimeValue[A] :+: JavaUtilValueCo[A]
  type CustomStringValueCo[A] = CustomStringValue[A] :+: JavaTimeValueCo[A]

  val defaultResultSetValues: ResultSetValue[DefaultValues] = {
    ResultSetValue.merge[ScalaCoreValue, CustomStringValueCo](
      DefaultScalaCoreResultSet,
      ResultSetValue.merge[CustomStringValue, JavaTimeValueCo](
        DefaultCustomStringResultSet,
        ResultSetValue.merge[JavaTimeValue, JavaUtilValueCo](
          DefaultJavaTimeResultSet,
          ResultSetValue
            .merge[JavaUtilValue, CNilF](
              DefaultJavaUtilResultSet,
              ResultSetValue.CNilResultSetValue
            )
        )
      )
    )
  }

  // end 2.12

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
    path: Path[String]
  ): Either[ExtractionErrors[String], CanBeOmitted[String, A]] =
    try {
      val result = f
      if (result == null) {
        Right(Left(List(OmittedValue(fieldName, typeName, path))))
      } else {
        Right(Right(result))
      }
    } catch {
      case ex: SQLException =>
        Left(List(SystemError(path, ex, Some(s"Error extracting field: ${fieldName}"))))
    }

}
