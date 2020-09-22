package com.bones.jdbc

import java.sql.PreparedStatement

import com.bones.data.values.DefaultValues
import com.bones.jdbc.DbUtil.camelToSnake
import com.bones.jdbc.column.ColumnNameInterpreter
import com.bones.jdbc.rs.ResultSetInterpreter

package object insert {

  type FieldName = String
  type FieldValue = String
  type Key = String
  type ColumnName = String
  type SetValue = PreparedStatement => Unit
  type SetNull = PreparedStatement => Unit
  type Index = Int
  type InsertPair[A] = Key => (Index, A) => (Index, List[(ColumnName, SetValue)])

  object DefaultScalaCoreDbInsert extends ScalaCoreDbInsert
  object DefaultJavaUtilDbInsert extends JavaUtilDbInsert
  object DefaultJavaTimeDbInsert extends JavaTimeDbInsert
  object DefaultCustomStringDbInsert extends CustomStringDbInsert

  val defaultDbInsertValues: DbInsertValue[DefaultValues] =
    DefaultScalaCoreDbInsert ++
      (DefaultCustomStringDbInsert ++
        (DefaultJavaTimeDbInsert ++
          (DefaultJavaUtilDbInsert ++ CNilInsertInterpreter)))

  val defaultDbInsertInterpreter: DbInsert[DefaultValues] = new DbInsert[DefaultValues] {
    override def resultSetInterpreter: ResultSetInterpreter[DefaultValues] =
      com.bones.jdbc.rs.defaultResultSetInterpreter

    override def customInterpreter: DbInsertValue[DefaultValues] =
      com.bones.jdbc.insert.defaultDbInsertValues

    override def columnNameInterpreter: ColumnNameInterpreter[DefaultValues] =
      com.bones.jdbc.column.defaultColumnNameInterpreter
  }

  /** Create the return type for valueDefinition given the arguments */
  def psF[A](f: (PreparedStatement, Index, A) => Unit): InsertPair[A] =
    key => {
      val columnName = camelToSnake(key)
      (index: Index, a: A) =>
        {
          val setValue: SetValue = ps => {
            f(ps, index, a)
          }
          (index + 1, List((columnName, setValue)))
        }
    }

}
