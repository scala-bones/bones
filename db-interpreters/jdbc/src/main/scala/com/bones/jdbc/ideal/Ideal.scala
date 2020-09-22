package com.bones.jdbc.ideal

import cats.implicits.catsSyntaxSemigroup
import com.bones.data.KvpCollection.headTypeName
import com.bones.data.template.KvpCollectionMatch
import com.bones.data.{HigherOrderTemplate, _}
import com.bones.jdbc.{DbUtil, hasUniqueConstraint}
import com.bones.jdbc.DbUtil.camelToSnake
import com.bones.si.ideal.{IdealColumn, StringType}
import com.bones.validation.ValidationDefinition.{UniqueValue, ValidationOp}
import shapeless.{::, Coproduct, HList, Nat}

trait Ideal[ALG[_]]
    extends KvpCollectionMatch[
      ALG,
      (TableCollection, Option[ColumnName], Option[Description]) => TableCollection] {
  self =>

  def algInterpreter: IdealValue[ALG]

  val addColumnToWorkingTable = new HigherOrderTemplate[
    ALG,
    (TableCollection, ColumnName, Option[Description]) => TableCollection] {

    override protected def optionalToOut[B](opt: OptionalValue[ALG, B])
      : (TableCollection, ColumnName, Option[Description]) => TableCollection = {
      opt.valueDefinitionOp match {
        case Left(higherOrderValue) =>
          (collection: TableCollection, columnName: String, description: Option[String]) =>
            nullableNewColumns(collection)(
              fromConcreteValue(higherOrderValue)(collection, columnName, description))
        case Right(value) =>
          (collection: TableCollection, columnName: String, description: Option[String]) =>
            nullableNewColumns(collection)(
              algInterpreter
                .columns(value)(collection, columnName, description))
      }
    }

    private def nullableNewColumns(collection: TableCollection)(f: => TableCollection) = {
      val oldColumns = collection.activeTable.columns
      val subCollection = f

      //Find new columns and make them optional
      val newColumns =
        subCollection.activeTable.columns.filterNot(col => oldColumns.contains(col))
      val optionalColumns = newColumns.map(_.copy(nullable = true))
      val newActiveTable =
        subCollection.activeTable.copy(columns = oldColumns ::: optionalColumns)
      subCollection.copy(activeTable = newActiveTable)
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
      : (TableCollection, ColumnName, Option[Description]) => TableCollection = {
      val fa = determineValueDefinition(either.definitionA)
      val fb = determineValueDefinition(either.definitionB)

      (tc, columnName, desc) =>
        {
          val a = fa.apply(tc, columnName, desc)
          //find new columns
          val aColumns =
            a.activeTable.columns.filterNot(col => tc.activeTable.columns.exists(_ eq col))
          val b = fb.apply(a, columnName, desc)
          val bColumns =
            b.activeTable.columns.filterNot(col => a.activeTable.columns.exists(_ eq col))

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

          b.copy(activeTable = b.activeTable.copy(columns = newColumnsOptional))

        }

    }

    override protected def listToOut[A](list: ListData[ALG, A])
      : (TableCollection, ColumnName, Option[Description]) => TableCollection = {
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
    override protected def kvpCollectionToOut[A](hList: KvpCollectionValue[ALG, A])
      : (TableCollection, ColumnName, Option[Description]) => TableCollection = {

      (tableCollection: TableCollection, columnName: String, description: Option[String]) =>
        fromKvpCollection(hList.kvpCollection)(tableCollection, Some(columnName), description)
    }
  }

  override def kvpNil(kvp: KvpNil[ALG])
    : (TableCollection, Option[ColumnName], Option[Description]) => TableCollection =
    (tc, _, _) => tc

  override def kvpHListCollectionHead[
    HO <: HList,
    NO <: Nat,
    H <: HList,
    HL <: Nat,
    T <: HList,
    TL <: Nat](kvp: KvpHListCollectionHead[ALG, HO, NO, H, HL, T, TL])
    : (TableCollection, Option[ColumnName], Option[Description]) => TableCollection = {
    val head = fromKvpCollection(kvp.head)
    val tail = fromKvpCollection(kvp.tail)
    (tableCollection: TableCollection, columnName, description) =>
      {
        val headTableCollection = head(tableCollection, columnName, description)
        tail(headTableCollection, columnName, description)
      }
  }

  override def kvpWrappedHList[A, H <: HList, HL <: Nat](
    wrappedHList: KvpWrappedHList[ALG, A, H, HL])
    : (TableCollection, Option[ColumnName], Option[Description]) => TableCollection =
    fromKvpCollection(wrappedHList.wrappedEncoding)

  override def kvpWrappedCoproduct[A, C <: Coproduct](
    wrappedCoproduct: KvpWrappedCoproduct[ALG, A, C])
    : (TableCollection, Option[ColumnName], Option[Description]) => TableCollection = {
    (tc, columnName, description) =>
      {
        val result =
          fromKvpCollection(wrappedCoproduct.wrappedEncoding)(tc, columnName, description)
        val dtypeColumn = IdealColumn(
          "dtype",
          StringType.unbounded,
          false,
          Some(s"Column to specify sub type of ${wrappedCoproduct.typeNameOfA}")
        )
        val at = result.activeTable
        val newTable = at.copy(columns = dtypeColumn :: at.columns)
        result.copy(activeTable = newTable)
      }
  }

  override def kvpSingleValueHead[H, T <: HList, TL <: Nat, O <: H :: T](
    kvp: KvpSingleValueHead[ALG, H, T, TL, O])
    : (TableCollection, Option[ColumnName], Option[Description]) => TableCollection = {
    val headResult = kvp.head match {
      case Left(keyDef) => {
        (tc: TableCollection, columnName: Option[ColumnName], description: Option[Description]) =>
          val camelKey = camelToSnake(keyDef.key)
          val newColumnName = columnName.fold(camelKey)(cn => cn + "_" + camelKey)
          val newDescription = description |+| keyDef.description
          determineValueDefinition(keyDef.dataDefinition)(tc, newColumnName, newDescription)
      }
      case Right(kvpColl) => fromKvpCollection(kvpColl)
    }
    val tailResult = fromKvpCollection(kvp.tail)
    (tc, cn, de) =>
      {
        val headTc = headResult(tc, cn, de)
        tailResult(headTc, cn, de)
      }
  }

  def determineValueDefinition[A](dataDefinition: Either[HigherOrderValue[ALG, A], ALG[A]])
    : (TableCollection, ColumnName, Option[Description]) => TableCollection = {
    dataDefinition match {
      case Left(higherOrderValue) =>
        (tc, name, desc) =>
          addColumnToWorkingTable.fromConcreteValue(higherOrderValue)(tc, name, desc)
      case Right(alg) => algInterpreter.columns(alg)
    }
  }

  override def kvpCoproduct[C <: Coproduct](value: KvpCoproduct[ALG, C])
    : (TableCollection, Option[ColumnName], Option[Description]) => TableCollection = {

    value match {
      case _: KvpCoNil[ALG] =>
        (tc, _, _) =>
          tc
      case kvp: KvpCoproductCollectionHead[ALG, a, C, o] @unchecked => {
        val head = fromKvpCollection(kvp.kvpCollection)
        val tail = kvpCoproduct(kvp.kvpTail)
        (tc, cn, de) =>
          {
            val headTc = head(tc, cn, de)
            val tailTc = tail(headTc, cn, de)

            // columns that are not in both head and tail become optional or
            // if tail is CNil and has no columns, then we will not mark anything as optional
            val newHeadColumns =
              headTc.activeTable.columns.filterNot(col => tc.activeTable.columns.exists(_ eq col))

            val newTailColumns =
              tailTc.activeTable.columns.filterNot(col =>
                headTc.activeTable.columns.exists(_ eq col))

            if (newTailColumns.isEmpty) {
              tailTc
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
              tailTc.copy(activeTable = newActiveTable)
            }

          }
      }
    }

  }

  def toIdeal[A: Manifest](
    schema: KvpCollection[ALG, A],
    name: Option[String] = None,
    description: Option[String] = None): TableCollection = {
    val tableName =
      DbUtil.camelToSnake(headTypeName(schema).getOrElse("Unknown"))
    val tableCol = TableCollection.init(tableName, description)
    fromKvpCollection(schema).apply(tableCol, None, None)
  }

}
