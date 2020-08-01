package com.bones.liquibase

import com.bones.liquibase.GenerateDatabaseObject.{Name, TableItems}
import liquibase.change.ColumnConfig
import liquibase.structure.core.{Column, Table}

package object value {

  object DefaultScalaCoreDatabaseObject extends ScalaCoreDatabaseObject


  def asResult(config: ColumnConfig, name: String) : (List[(Table, TableItems)], List[TableItems]) = {
    config.setName(name)
    val column = new Column(config)
    (List.empty, List(TableItems(List(column), List.empty, List.empty, List.empty)))
  }
}
