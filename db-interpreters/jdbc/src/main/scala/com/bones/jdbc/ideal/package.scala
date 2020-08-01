package com.bones.jdbc

import com.bones.si.ideal.{IdealColumn, IdealForeignKey, IdealTable}

package object ideal {

  object TableCollection {
    def init(name: String, description: Option[String]): TableCollection = {
      val table = IdealTable.empty(name, description)
      TableCollection(table, List.empty)
    }
  }

  /**
   * Responsible for keeping track of the "Ideal" being created.
   * @param activeTable The current active table.  If you are adding a column, add it to this one.
   * @param otherTables The list of sub tables.
   */
  case class TableCollection(activeTable: IdealTable, otherTables: List[IdealTable]) {
    def prependPrimaryKey(column: IdealColumn): TableCollection = {
      val newActiveTable = activeTable.copy(primaryKeyColumns = column :: activeTable.primaryKeyColumns)
      TableCollection(newActiveTable, otherTables)
    }
    def prependColumn(column: IdealColumn): TableCollection = {
      val newActiveTable = activeTable.copy(columns = column :: activeTable.columns)
      TableCollection(newActiveTable, otherTables)
    }
    def prependForeignKey(foreignKey: IdealForeignKey): TableCollection = {
      val newActiveTable= activeTable.copy(foreignKeys = foreignKey :: activeTable.foreignKeys)
      TableCollection(newActiveTable, otherTables)
    }
    def startNewTable(name: String, description: Option[String]): TableCollection = {
      val table = IdealTable.empty(name, description)
      copy(otherTables = activeTable :: otherTables, activeTable = table)
    }
    def prepend(other: TableCollection): TableCollection = {
      if (other.activeTable.name == activeTable.name) {
        val newActiveTable = activeTable.copy(columns = other.activeTable.columns ::: activeTable.columns)
          .copy(primaryKeyColumns = other.activeTable.primaryKeyColumns ::: activeTable.primaryKeyColumns)
          .copy(foreignKeys = other.activeTable.foreignKeys ::: activeTable.foreignKeys)
        val newOtherTables = other.otherTables.filterNot(table => otherTables.contains(table))
        val allOtherTables = newOtherTables ::: otherTables
        TableCollection(newActiveTable, allOtherTables)
      } else {
        val newOtherTables = other.otherTables.filterNot(table => otherTables.contains(table))
        val allOtherTables = activeTable :: newOtherTables ::: otherTables
        TableCollection(other.activeTable, allOtherTables)
      }

    }

  }

  type ColumnName = String
  type Description = String

  trait IdealValue[ALG[_]] {
    def columns[A](alg: ALG[A]): (TableCollection, ColumnName, Option[Description]) => TableCollection
  }
}
