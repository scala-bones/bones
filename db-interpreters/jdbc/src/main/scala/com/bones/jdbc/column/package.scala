package com.bones.jdbc

import com.bones.data.KvpCoproduct
import com.bones.data.values.{
  CNilF,
  CustomStringValue,
  DefaultValues,
  JavaTimeValue,
  JavaUtilValue,
  ScalaCoreValue
}
import com.bones.jdbc.column.ColumnValue.CNilColumnValue
import shapeless.{:+:, Coproduct}

package object column {

  type ColumnName = String
  type Key = String
  case class Column(name: String, columnDefinition: String, nullable: Boolean)
  case class Table(name: String, columns: List[Column])
  type ToColumns = Key => List[Column]

  def nameToColumn(columnDefinition: String): ToColumns =
    name => List(Column(DbUtil.camelToSnake(name), columnDefinition, false))

//  val defaultDbColumnValues: ColumnValue[DefaultValues] =
//    (DefaultScalaCoreDbColumnValue ++
//      (DefaultColumnStringDbValue ++
//        (DefaultJavaTimeDbColumnValue ++
//          (DefaultJavaUtilDbColumnValue ++ CNilColumnValue))))

  // Below is equivalent to the above.  Above compiles in 2.13, below compiles in both 2.12 and 2.13
  // start 2.12

  type JavaUtilValueCo[A] = JavaUtilValue[A] :+: CNilF[A]
  type JavaTimeValueCo[A] = JavaTimeValue[A] :+: JavaUtilValueCo[A]
  type CustomStringValueCo[A] = CustomStringValue[A] :+: JavaTimeValueCo[A]

  val defaultDbColumnValues: ColumnValue[DefaultValues] = {
    ColumnValue.merge[ScalaCoreValue, CustomStringValueCo](
      DefaultScalaCoreDbColumnValue,
      ColumnValue.merge[CustomStringValue, JavaTimeValueCo](
        DefaultColumnStringDbValue,
        ColumnValue.merge[JavaTimeValue, JavaUtilValueCo](
          DefaultJavaTimeDbColumnValue,
          ColumnValue
            .merge[JavaUtilValue, CNilF](DefaultJavaUtilDbColumnValue, CNilColumnValue)
        )
      )
    )
  }

  // end 2.12

  object DefaultJavaTimeDbColumnValue extends JavaTimeDbColumnValue
  object DefaultJavaUtilDbColumnValue extends JavaUtilDbColumnValue
  object DefaultScalaCoreDbColumnValue extends ScalaCoreDbColumnValue
  object DefaultColumnStringDbValue extends ColumnStringDbColumnValue

  val defaultDbColumnInterpreter: DbColumnInterpreter[DefaultValues] =
    new DbColumnInterpreter[DefaultValues] {
      override def customInterpreter: ColumnValue[DefaultValues] = defaultDbColumnValues
    }

  object defaultColumnNameInterpreter extends ColumnNameInterpreter[DefaultValues]

}
