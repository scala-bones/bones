package com.bones.jdbc.ideal

import cats.implicits.catsSyntaxSemigroup
import com.bones.data.KvpCollection.headTypeName
import com.bones.data.template.KvpCollectionMatch
import com.bones.data.{HigherOrderTemplate, _}
import com.bones.jdbc.DbUtil
import com.bones.jdbc.DbUtil.camelToSnake
import com.bones.si.ideal.{IdealColumn, StringType, UniqueConstraint}
import shapeless.{::, Coproduct, HList, Nat}

/**
  *
  * There are three ways a column can be part of a group.
  *   1. It can be solo unique, which is a unique constraint on a value w/o a group name
  *    or with a group name, but without a partner.
  *   2.It can be grouped unique, which has a group name and a partner.
  *   3.It can be part of a larger unique construct, such as if an entire HList or Class is designated as unique.
  *
  *   For 1 and 2 above, the IdealColumn wil be added
  *      to the UniqueGroup list at the time the IdealColumn is created in the Value Interpreter.
  *   For 3 above, we will search for all new columns added to the child of a group and add those
  *       columns to the UniqueGroup list.
  *
  * @tparam ALG
  */
trait Ideal[ALG[_]]
    extends KvpCollectionMatch[
      String,
      ALG,
      (TableCollection, List[UniqueGroup], Option[ColumnName], Option[Description]) => (
        TableCollection,
        List[UniqueGroup])] {
  self =>

  def algInterpreter: IdealValue[ALG]

  val addColumnToWorkingTable = new HigherOrderTemplate[
    String,
    ALG,
    (TableCollection, List[UniqueGroup], ColumnName, Option[Description]) => (
      TableCollection,
      List[UniqueGroup])] {

    override protected def optionalToOut[B](opt: OptionalValue[ALG, B])
      : (TableCollection, List[UniqueGroup], ColumnName, Option[Description]) => (
        TableCollection,
        List[UniqueGroup]) = {
      opt.valueDefinitionOp match {
        case Left(higherOrderValue) =>
          (
            collection: TableCollection,
            ug: List[UniqueGroup],
            columnName: String,
            description: Option[String]) =>
            nullableNewColumns(collection)(
              fromConcreteValue(higherOrderValue)(collection, ug, columnName, description))
        case Right(value) =>
          (
            collection: TableCollection,
            ug: List[UniqueGroup],
            columnName: String,
            description: Option[String]) =>
            nullableNewColumns(collection)(
              algInterpreter
                .columns(value)(collection, ug, columnName, description))
      }
    }

    private def nullableNewColumns(collection: TableCollection)(
      f: => (TableCollection, List[UniqueGroup])) = {
      val oldColumns = collection.activeTable.columns
      val (subCollection, uGroups) = f

      //Find new columns and make them optional
      val newColumns =
        subCollection.activeTable.columns.filterNot(col => oldColumns.contains(col))
      val optionalColumns = newColumns.map(_.copy(nullable = true))
      val newActiveTable =
        subCollection.activeTable.copy(columns = oldColumns ::: optionalColumns)
      (subCollection.copy(activeTable = newActiveTable), uGroups)
    }

    /**
      * If Either is a top-level entry point, then we will create two tables.  If it is not, we will
      * combine the two resulting tables, passing the names 'left' and 'right' as prefixes to the column names.
      * @param either The data describing the either.
      * @tparam A The left type.
      * @tparam B The right type
      * @return A function to create the tables.
      */
    override protected def eitherToOut[A, B](either: EitherData[ALG, A, B])
      : (TableCollection, List[UniqueGroup], ColumnName, Option[Description]) => (
        TableCollection,
        List[UniqueGroup]) = {
      val fa = determineValueDefinition(either.definitionA)
      val fb = determineValueDefinition(either.definitionB)

      (tc, ug, columnName, desc) =>
        {
          val (aTc, aUg) = fa.apply(tc, ug, columnName, desc)
          //find new columns
          val aColumns =
            aTc.activeTable.columns.filterNot(col => tc.activeTable.columns.exists(_ eq col))
          val (bTc, bUg) = fb.apply(aTc, aUg, columnName, desc)
          val bColumns =
            bTc.activeTable.columns.filterNot(col => aTc.activeTable.columns.exists(_ eq col))

          //If there are duplicate names, then we will append the name of the type
          val (aMatching, aUnique) =
            aColumns.partition(colA => bColumns.exists(colB => colA.name == colB.name))
          val (bMatching, bUnique) =
            bColumns.partition(colB => aColumns.exists(colA => colA.name == colB.name))

          val newColumns =
            aMatching.map(col => col.copy(name = col.name + "_" + camelToSnake(either.typeNameOfA))) :::
              aUnique :::
              bMatching.map(col =>
              col.copy(name = col.name + "_" + camelToSnake(either.typeNameOfB))) :::
              bUnique

          // Since this is an either, all columns become optional.
          val newColumnsOptional = newColumns.map(col => col.copy(nullable = true))

          (bTc.copy(activeTable = bTc.activeTable.copy(columns = newColumnsOptional)), bUg)

        }

    }

    override protected def listToOut[A](list: ListData[ALG, A])
      : (TableCollection, List[UniqueGroup], ColumnName, Option[Description]) => (
        TableCollection,
        List[UniqueGroup]) = {
      list.tDefinition match {
        case Left(col)  => fromConcreteValue(col)
        case Right(alg) => algInterpreter.columns(alg)
      }
    }

    /**
      * Create a function, where it he column name is provided, we will create a new table collection.
      * @param hList
      * @return
      */
    override protected def kvpCollectionToOut[A](hList: KvpCollectionValue[String, ALG, A])
      : (TableCollection, List[UniqueGroup], ColumnName, Option[Description]) => (
        TableCollection,
        List[UniqueGroup]) = {

      (
        tableCollection: TableCollection,
        ug: List[UniqueGroup],
        columnName: String,
        description: Option[String]) =>
        fromKvpCollection(hList.kvpCollection)(tableCollection, ug, Some(columnName), description)
    }
  }

  override def kvpNil(kvp: KvpNil[String, ALG])
    : (TableCollection, List[UniqueGroup], Option[ColumnName], Option[Description]) => (
      TableCollection,
      List[UniqueGroup]) =
    (tc, ug, _, _) => (tc, ug)

  override def kvpHListCollectionHead[
    HO <: HList,
    NO <: Nat,
    H <: HList,
    HL <: Nat,
    T <: HList,
    TL <: Nat](kvp: KvpHListCollectionHead[String, ALG, HO, NO, H, HL, T, TL])
    : (TableCollection, List[UniqueGroup], Option[ColumnName], Option[Description]) => (
      TableCollection,
      List[UniqueGroup]) = {
    val head = fromKvpCollection(kvp.head)
    val tail = fromKvpCollection(kvp.tail)
    (tableCollection: TableCollection, ug: List[UniqueGroup], columnName, description) =>
      {
        val (headTc, headUg) = head(tableCollection, ug, columnName, description)
        tail(headTc, headUg, columnName, description)
      }
  }

  override def kvpWrappedHList[A, H <: HList, HL <: Nat](
    wrappedHList: KvpWrappedHList[String, ALG, A, H, HL])
    : (TableCollection, List[UniqueGroup], Option[ColumnName], Option[Description]) => (
      TableCollection,
      List[UniqueGroup]) =
    fromKvpCollection(wrappedHList.wrappedEncoding)

  override def kvpWrappedCoproduct[A, C <: Coproduct](
    wrappedCoproduct: KvpWrappedCoproduct[String, ALG, A, C])
    : (TableCollection, List[UniqueGroup], Option[ColumnName], Option[Description]) => (
      TableCollection,
      List[UniqueGroup]) = { (tc, ug, columnName, description) =>
    {
      val (kvpTc, kvpUg) =
        fromKvpCollection(wrappedCoproduct.wrappedEncoding)(tc, ug, columnName, description)
      val dtypeColumn = IdealColumn(
        "dtype",
        StringType.unbounded,
        false,
        Some(s"Column to specify sub type of ${wrappedCoproduct.typeNameOfA}")
      )
      val at = kvpTc.activeTable
      val newTable = at.copy(columns = dtypeColumn :: at.columns)
      (tc.copy(activeTable = newTable), kvpUg)
    }
  }

  override def kvpSingleValueHead[H, T <: HList, TL <: Nat, O <: H :: T](
    kvp: KvpSingleValueHead[String, ALG, H, T, TL, O])
    : (TableCollection, List[UniqueGroup], Option[ColumnName], Option[Description]) => (
      TableCollection,
      List[UniqueGroup]) = {
    val headResult = kvp.head match {
      case Left(keyDef) => {
        (
          tc: TableCollection,
          ug: List[UniqueGroup],
          columnName: Option[ColumnName],
          description: Option[Description]) =>
          val camelKey = camelToSnake(keyDef.key)
          val newColumnName = columnName.fold(camelKey)(cn => cn + "_" + camelKey)
          val newDescription = description |+| keyDef.description
          determineValueDefinition(keyDef.dataDefinition)(tc, ug, newColumnName, newDescription)
      }
      case Right(kvpColl) => fromKvpCollection(kvpColl)
    }
    val tailResultF = fromKvpCollection(kvp.tail)
    (tc, ug, cn, de) =>
      {
        val (headTc, headUg) = headResult(tc, ug, cn, de)
        tailResultF(headTc, headUg, cn, de)
      }
  }

  def determineValueDefinition[A](dataDefinition: Either[HigherOrderValue[ALG, A], ALG[A]])
    : (TableCollection, List[UniqueGroup], ColumnName, Option[Description]) => (
      TableCollection,
      List[UniqueGroup]) = {
    dataDefinition match {
      case Left(higherOrderValue) =>
        (tc, ug, name, desc) =>
          addColumnToWorkingTable.fromConcreteValue(higherOrderValue)(tc, ug, name, desc)
      case Right(alg) => algInterpreter.columns(alg)
    }
  }

  override def kvpCoproduct[C <: Coproduct](value: KvpCoproduct[String, ALG, C])
    : (TableCollection, List[UniqueGroup], Option[ColumnName], Option[Description]) => (
      TableCollection,
      List[UniqueGroup]) = {

    value match {
      case _: KvpCoNil[String, ALG] =>
        (tc, ug, _, _) =>
          (tc, ug)
      case kvp: KvpCoproductCollectionHead[String, ALG, a, C, o] @unchecked => {
        val head = fromKvpCollection(kvp.kvpCollection)
        val tail = kvpCoproduct(kvp.kvpTail)
        (tc, ug, cn, de) =>
          {
            val (headTc, headUg) = head(tc, ug, cn, de)
            val (tailTc, tailUg) = tail(headTc, headUg, cn, de)

            // columns that are not in both head and tail become optional or
            // if tail is CNil and has no columns, then we will not mark anything as optional
            val newHeadColumns =
              headTc.activeTable.columns.filterNot(col => tc.activeTable.columns.exists(_ eq col))

            val newTailColumns =
              tailTc.activeTable.columns.filterNot(col =>
                headTc.activeTable.columns.exists(_ eq col))

            if (newTailColumns.isEmpty) {
              (tailTc, tailUg)
            } else {
              val (uniqueHeadColumns, headSharedColumns) =
                newHeadColumns.partition(col => newTailColumns.exists(_.name == col.name))
              val (uniqueTailColumns, tailSharedColumns) =
                newTailColumns.partition(col => newHeadColumns.exists(_.name == col.name))

              val newColumns = {
                tc.activeTable.columns :::
                  uniqueHeadColumns :::
                  headSharedColumns.map(_.copy(nullable = true)) :::
                  uniqueTailColumns :::
                  tailSharedColumns.map(_.copy(nullable = true)) :::
                  Nil
              }
              val newActiveTable = tailTc.activeTable.copy(columns = newColumns)
              (tailTc.copy(activeTable = newActiveTable), tailUg)
            }

          }
      }
    }

  }

  def toIdeal[A: Manifest, ID](
    idSchema: KvpCollection[String, ALG, ID],
    schema: KvpCollection[String, ALG, A],
    name: Option[String] = None,
    description: Option[String] = None): TableCollection = {
    val tableName =
      DbUtil.camelToSnake(headTypeName(schema).getOrElse("Unknown"))
    val tableCol = TableCollection.init(tableName, description)
    val (idTc, _) = fromKvpCollection(idSchema)(tableCol, List.empty, None, None)
    // all columns are primary key columns
    val idTableCol = {
      val at = idTc.activeTable
      val newActiveTable = at.copy(primaryKeyColumns = at.allColumns).copy(columns = List.empty)
      tableCol.copy(activeTable = newActiveTable)
    }
    val (tc, ug) = fromKvpCollection(schema).apply(idTableCol, List.empty, None, None)

    ug.foldLeft(tc) {
      case (tc, ug) => {
        val uc = UniqueConstraint(ug.columns)

        // Find table for which the unique constraints belongs and update
        if (tc.activeTable.name == ug.tableName) {
          val newTable =
            tc.activeTable.copy(uniqueConstraints = uc :: tc.activeTable.uniqueConstraints)
          tc.copy(activeTable = newTable)
        } else {
          val (tables, otherTables) = tc.otherTables.partition(_.name == ug.tableName)
          val newTables = tables.map(t => t.copy(uniqueConstraints = uc :: t.uniqueConstraints))
          tc.copy(otherTables = otherTables ::: newTables)
        }

      }
    }

  }

}
