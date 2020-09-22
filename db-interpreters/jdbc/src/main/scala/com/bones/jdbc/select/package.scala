package com.bones.jdbc

import com.bones.data.values.DefaultValues
import com.bones.jdbc.column.ColumnNameInterpreter
import com.bones.jdbc.rs.ResultSetInterpreter
import com.bones.jdbc.update.JdbcStatementInterpreter

package object select {
  val defaultSelectInterpreter = new SelectInterpreter[DefaultValues] {
    override def columnNameInterpreter: ColumnNameInterpreter[DefaultValues] =
      com.bones.jdbc.column.defaultColumnNameInterpreter

    override def jdbcStatementInterpreter: JdbcStatementInterpreter[DefaultValues] =
      com.bones.jdbc.update.defaultJdbcStatementInterpreter

    override def resultSetInterpreter: ResultSetInterpreter[DefaultValues] =
      com.bones.jdbc.rs.defaultResultSetInterpreter
  }
}
