package com.bones.jdbc

import java.sql.PreparedStatement

import com.bones.data.values.{
  CNilF,
  CustomStringValue,
  DefaultValues,
  JavaTimeValue,
  JavaUtilValue,
  ScalaCoreValue
}
import com.bones.jdbc.DbUtil.camelToSnake
import com.bones.jdbc.column.ColumnNameInterpreter
import com.bones.jdbc.rs.ResultSetInterpreter
import shapeless.:+:

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

//  val defaultDbInsertValues: DbInsertValue[DefaultValues] =
//    DefaultScalaCoreDbInsert ++
//      (DefaultCustomStringDbInsert ++
//        (DefaultJavaTimeDbInsert ++
//          (DefaultJavaUtilDbInsert ++ CNilInsertInterpreter)))

  // Below is equivalent to the above.  Above compiles in 2.13, below compiles in both 2.12 and 2.13
  // start 2.12

  type JavaUtilValueCo[A] = JavaUtilValue[A] :+: CNilF[A]
  type JavaTimeValueCo[A] = JavaTimeValue[A] :+: JavaUtilValueCo[A]
  type CustomStringValueCo[A] = CustomStringValue[A] :+: JavaTimeValueCo[A]

  val defaultDbInsertValues: DbInsertValue[DefaultValues] = {
    DbInsertValue.merge[ScalaCoreValue, CustomStringValueCo](
      DefaultScalaCoreDbInsert,
      DbInsertValue.merge[CustomStringValue, JavaTimeValueCo](
        DefaultCustomStringDbInsert,
        DbInsertValue.merge[JavaTimeValue, JavaUtilValueCo](
          DefaultJavaTimeDbInsert,
          DbInsertValue
            .merge[JavaUtilValue, CNilF](DefaultJavaUtilDbInsert, CNilInsertInterpreter)
        )
      )
    )
  }

  // end 2.12

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
      (index: Index, a: A) => {
        val setValue: SetValue = ps => {
          f(ps, index, a)
        }
        (index + 1, List((columnName, setValue)))
      }
    }

}
