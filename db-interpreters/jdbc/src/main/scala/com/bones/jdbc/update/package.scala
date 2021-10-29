package com.bones.jdbc

import java.sql.PreparedStatement

import com.bones.data.KvpCoproduct
import com.bones.data.values.{
  CNilF,
  CustomStringValue,
  DefaultValues,
  JavaTimeValue,
  JavaUtilValue,
  ScalaCoreValue
}
import com.bones.jdbc.DbUtil.camelToSnake
import com.bones.jdbc.rs.{DefaultCustomStringResultSet, DefaultJavaTimeResultSet}
import com.bones.jdbc.update.UpdateStatementValue.CNilUpdateStatementInterpreter$
import shapeless.{:+:, Coproduct}

package object update {

  type FieldName = String
  type FieldValue = String
  type Key = String
  type AssignmentString = String
  type SetValue = PreparedStatement => Unit
  type SetNull = PreparedStatement => Unit
  type Index = Int
  type ID = Long

  case class JdbcColumnStatement[A](
    lastIndex: Index,
    assignmentStatements: List[(AssignmentString, SetNull)],
    predicates: A => List[SetValue]
  )

  object DefaultJavaTimeUpdateStatement extends JavaTimeUpdateStatement
  object DefaultJavaUtilUpdateStatement extends JavaUtilUpdateStatement
  object DefaultScalaCoreUpdateStatement extends ScalaCoreUpdateStatement
  object DefaultCustomStringUpdateStatement extends CustomStringUpdateStatement

//  val defaultUpdateStatementInterpreter: UpdateStatementValue[DefaultValues] =
//    DefaultScalaCoreUpdateStatement ++
//      (DefaultCustomStringUpdateStatement ++
//        (DefaultJavaTimeUpdateStatement ++
//          (DefaultJavaUtilUpdateStatement ++ CNilUpdateStatementInterpreter$)))

  // Below is equivalent to the above.  Above compiles in 2.13, below compiles in both 2.12 and 2.13
  // start 2.12

  type JavaUtilValueCo[A] = JavaUtilValue[A] :+: CNilF[A]
  type JavaTimeValueCo[A] = JavaTimeValue[A] :+: JavaUtilValueCo[A]
  type CustomStringValueCo[A] = CustomStringValue[A] :+: JavaTimeValueCo[A]

  val defaultUpdateStatementInterpreter: UpdateStatementValue[DefaultValues] = {
    UpdateStatementValue.merge[ScalaCoreValue, CustomStringValueCo](
      DefaultScalaCoreUpdateStatement,
      UpdateStatementValue.merge[CustomStringValue, JavaTimeValueCo](
        DefaultCustomStringUpdateStatement,
        UpdateStatementValue.merge[JavaTimeValue, JavaUtilValueCo](
          DefaultJavaTimeUpdateStatement,
          UpdateStatementValue
            .merge[JavaUtilValue, CNilF](
              DefaultJavaUtilUpdateStatement,
              CNilUpdateStatementInterpreter$
            )
        )
      )
    )
  }

  // end 2.12

  val defaultJdbcStatementInterpreter = new JdbcStatementInterpreter[DefaultValues] {
    override def customDbUpdateInterpreter: UpdateStatementValue[DefaultValues] =
      defaultUpdateStatementInterpreter
  }

  val defaultDbUpdate = new DbUpdate[DefaultValues] {
    override def jdbcStatementInterpreter: JdbcStatementInterpreter[DefaultValues] =
      defaultJdbcStatementInterpreter
  }

  /** Create the return type for valueDefinition given the arguments */
  def psF[A](
    f: Index => (PreparedStatement, A) => Unit,
    sqlType: Int
  ): (Index, Key) => JdbcColumnStatement[A] =
    (index, key) => {
      val updateString = s"${camelToSnake(key)} = ?"
      val fI = f(index)
      val psNull: SetNull = ps => ps.setNull(index, sqlType)
      val setValue: A => List[SetValue] = a => {
        val setValueF: PreparedStatement => Unit = ps => fI(ps, a)
        List(setValueF)
      }
      JdbcColumnStatement(index + 1, List((updateString, psNull)), setValue)
    }

}
