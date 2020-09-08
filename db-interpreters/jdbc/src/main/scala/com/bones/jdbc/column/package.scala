package com.bones.jdbc

import com.bones.data.KvpCoproduct
import com.bones.data.values.DefaultValues
import com.bones.jdbc.column.ColumnValue.CNilColumnValue
import shapeless.Coproduct

package object column {

  type ColumnName = String
  type Key = String
  case class Column(name: String, columnDefinition: String, nullable: Boolean)
  case class Table(name: String, columns: List[Column])
  type ToColumns = Key => List[Column]

  def nameToColumn(columnDefinition: String): ToColumns =
    name => List(Column(DbUtil.camelToSnake(name), columnDefinition, false))

  val defaultDbColumnValues: ColumnValue[DefaultValues] =
    (DefaultScalaCoreDbColumnValue ++
      (DefaultColumnStringDbValue ++
        (DefaultJavaTimeDbColumnValue ++
          (DefaultJavaUtilDbColumnValue ++ CNilColumnValue))))

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
