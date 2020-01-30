package com.bones

import com.bones.jdbc.ColumnNameInterpreter.{ColumnName, Key}
import com.bones.jdbc.DbColumnInterpreter.ToColumns
import com.bones.jdbc.DbInsertValues.InsertPair
import com.bones.jdbc.DbUpdateValues.{CustomDbUpdateInterpreter, Index}
import com.bones.syntax.NoAlgebra

package object jdbc {
  import cats.data.NonEmptyList
  import java.sql.ResultSet

  trait JdbcCustomInterpreter[ALG[_]]
      extends ColumnNameInterpreter.CustomInterpreter[ALG]
      with ResultSetInterpreter.CustomInterpreter[ALG]
      with DbColumnInterpreter.CustomInterpreter[ALG]
      with DbInsertValues.CustomInterpreter[ALG]
      with CustomDbUpdateInterpreter[ALG]

  case object NoAlgebraJdbCustomInterpreter extends JdbcCustomInterpreter[NoAlgebra] {
    override def keyToColumnNames[A](alg: NoAlgebra[A]): Key => List[ColumnName] =
      sys.error("Unreachable code")

    override def toColumns[A](alg: NoAlgebra[A]): ToColumns =
      sys.error("Unreachable code")

    override def insertPair[A](alg: NoAlgebra[A]): InsertPair[A] =
      sys.error("Unreachable code")

    override def definitionResult[A](
      alg: NoAlgebra[A]): (Index, Key) => DbUpdateValues.DefinitionResult[A] =
      sys.error("Unreachable code")

    override def resultSet[A](
      alg: NoAlgebra[A]): (FindInterpreter.Path, FindInterpreter.FieldName) => ResultSet => Either[
      NonEmptyList[com.bones.data.Error.ExtractionError],
      A] =
      sys.error("Unreachable code")
  }

}
