package com.bones.jdbc

import com.bones.data.values.{
  CNilF,
  CustomStringValue,
  DefaultValues,
  JavaTimeValue,
  JavaUtilValue,
  ScalaCoreValue
}
import com.bones.jdbc.column.ColumnValue.CNilColumnValue
import com.bones.jdbc.column.{
  DefaultColumnStringDbValue,
  DefaultJavaTimeDbColumnValue,
  DefaultJavaUtilDbColumnValue,
  DefaultScalaCoreDbColumnValue
}
import com.bones.si.ideal.{IdealColumn, IdealDataType, IdealForeignKey, IdealTable}
import com.bones.validation.ValidationDefinition.UniqueValue
import shapeless.{:+:, Coproduct, Inl, Inr}

package object ideal {

  type UniqueUmbrella = List[String]
  case class UniqueGroup(tableName: String, name: Option[String], columns: List[IdealColumn])

//  val defaultIdealValue: IdealValue[DefaultValues] =
//    IdealScalaCoreInterpreter ++
//      (IdealCustomStringInterpreter ++
//        (IdealJavaTimeInterpreter ++
//          (IdealJavaUtilInterpreter ++ CNilIdealInterpreter)))

  // Below is equivalent to the above.  Above compiles in 2.13, below compiles in both 2.12 and 2.13
  //start 2.12

  type JavaUtilValueCo[A] = JavaUtilValue[A] :+: CNilF[A]
  type JavaTimeValueCo[A] = JavaTimeValue[A] :+: JavaUtilValueCo[A]
  type CustomStringValueCo[A] = CustomStringValue[A] :+: JavaTimeValueCo[A]

  val defaultIdealValue: IdealValue[DefaultValues] = {
    IdealValue.merge[ScalaCoreValue, CustomStringValueCo](
      IdealScalaCoreInterpreter,
      IdealValue.merge[CustomStringValue, JavaTimeValueCo](
        IdealCustomStringInterpreter,
        IdealValue.merge[JavaTimeValue, JavaUtilValueCo](
          IdealJavaTimeInterpreter,
          IdealValue
            .merge[JavaUtilValue, CNilF](IdealJavaUtilInterpreter, CNilIdealInterpreter)
        )
      )
    )
  }

  //end 2.12

  val defaultIdealInterpreter = new Ideal[DefaultValues] {
    override def algInterpreter: IdealValue[DefaultValues] = defaultIdealValue
  }

  object TableCollection {
    def init(name: String, description: Option[String]): TableCollection = {
      val table = IdealTable.empty(name, description)
      TableCollection(table, List.empty)
    }
  }

  /** Responsible for keeping track of the "Ideal" being created.
    * @param activeTable
    *   The current active table. If you are adding a column, add it to this one.
    * @param otherTables
    *   The list of sub tables.
    */
  case class TableCollection(activeTable: IdealTable, otherTables: List[IdealTable]) {

    def allTables: List[IdealTable] = activeTable :: otherTables
    def prependPrimaryKey(column: IdealColumn): TableCollection = {
      val newActiveTable =
        activeTable.copy(primaryKeyColumns = column :: activeTable.primaryKeyColumns)
      TableCollection(newActiveTable, otherTables)
    }
    def prependColumn(column: IdealColumn): TableCollection = {
      val newActiveTable = activeTable.copy(columns = column :: activeTable.columns)
      TableCollection(newActiveTable, otherTables)
    }
    def prependForeignKey(foreignKey: IdealForeignKey): TableCollection = {
      val newActiveTable = activeTable.copy(foreignKeys = foreignKey :: activeTable.foreignKeys)
      TableCollection(newActiveTable, otherTables)
    }
    def startNewTable(name: String, description: Option[String]): TableCollection = {
      val table = IdealTable.empty(name, description)
      copy(otherTables = activeTable :: otherTables, activeTable = table)
    }
    def prepend(other: TableCollection): TableCollection = {
      if (other.activeTable.name == activeTable.name) {
        val newActiveTable = activeTable
          .copy(columns = other.activeTable.columns ::: activeTable.columns)
          .copy(primaryKeyColumns =
            other.activeTable.primaryKeyColumns ::: activeTable.primaryKeyColumns
          )
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
  type TableName = String
  type Description = String

  object IdealValue {

    /** using kind projector allows us to create a new interpreter by merging two existing
      * interpreters. see https://stackoverflow.com/a/60561575/387094
      */
    def merge[L[_], R[_] <: Coproduct](
      li: IdealValue[L],
      ri: IdealValue[R]
    ): IdealValue[Lambda[A => L[A] :+: R[A]]] =
      new IdealValue[Lambda[A => L[A] :+: R[A]]] {
        override def columns[B](
          alg: L[B] :+: R[B]
        ): (TableCollection, List[UniqueGroup], ColumnName, Option[Description]) => (
          TableCollection,
          List[UniqueGroup]
        ) = {
          alg match {
            case Inl(l) => li.columns(l)
            case Inr(r) => ri.columns(r)
          }
        }

      }

    implicit class InterpreterOps[ALG[_]](val base: IdealValue[ALG]) extends AnyVal {
      def ++[R[_] <: Coproduct](
        r: IdealValue[R]
      ): IdealValue[Lambda[A => ALG[A] :+: R[A]]] =
        merge(base, r)

    }
  }

  trait IdealValue[ALG[_]] {
    def columns[A](
      alg: ALG[A]
    ): (TableCollection, List[UniqueGroup], ColumnName, Option[Description]) => (
      TableCollection,
      List[UniqueGroup]
    )
  }

  object CNilIdealInterpreter extends IdealValue[CNilF] {
    override def columns[A](
      alg: CNilF[A]
    ): (TableCollection, List[UniqueGroup], ColumnName, Option[Description]) => (
      TableCollection,
      List[UniqueGroup]
    ) =
      sys.error("Unreachable code")
  }

  def defaultColumns(
    newType: IdealDataType,
    uniqueConstraint: List[UniqueValue[_]]
  ): (TableCollection, List[UniqueGroup], ColumnName, Option[Description]) => (
    TableCollection,
    List[UniqueGroup]
  ) = { (tableCollection, uniqueGroups, name, description) =>
    {
      val newColumn = IdealColumn(name, newType, false, description)
      val newUg =
        uniqueConstraint.foldLeft(uniqueGroups)((groups, value) => {
          value.uniqueKey match {
            case Some(key) =>
              val (matching, others) = groups
                .partition(ug => ug.name.contains(key))
              val newMatching =
                matching.map(m => m.copy(columns = newColumn :: m.columns))
              if (newMatching.isEmpty)
                UniqueGroup(tableCollection.activeTable.name, Some(key), List(newColumn)) :: groups
              else
                newMatching ::: others

            case None =>
              UniqueGroup(tableCollection.activeTable.name, None, List(newColumn)) :: groups
          }
        })

      (tableCollection.prependColumn(newColumn), newUg)
    }
  }

}
