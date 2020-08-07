package com.bones.jdbc.ideal

import com.bones.data.values.InvalidStructureError
import com.bones.data.{ConcreteValueTemplate, _}
import com.bones.jdbc.DbUtil
import shapeless.{Coproduct, HList, Nat}

case class Ideal[ALG[_]](algInterpreter: IdealValue[ALG]) { self =>

  val tableFromConcreteValue = new ConcreteValueTemplate[
    ALG,
    TableCollection => Either[InvalidStructureError, TableCollection]] {
    override protected def optionalToOut[B: Manifest](
      opt: OptionalValue[ALG, B]): TableCollection => TableCollection =
      opt.valueDefinitionOp match {
        case Left(cv) => fromConcreteValue(cv)
        case Right(alg) =>
          _ =>
            Left(InvalidStructureError(s"unexpected algebra in optional: ${alg}"))
      }

    override protected def eitherToOut[A: Manifest, B: Manifest](either: EitherData[ALG, A, B])
      : TableCollection => Either[InvalidStructureError, TableCollection] =
      _ => Left(InvalidStructureError("either data currently not supported"))

    override protected def listToOut[A: Manifest](
      list: ListData[ALG, A]): TableCollection => Either[InvalidStructureError, TableCollection] =
      list.tDefinition match {
        case Left(cv) => fromConcreteValue(cv)
        case Right(alg) =>
          _ =>
            Left(InvalidStructureError(s"unexpected algebra in list : ${alg}"))
      }

    override protected def hListToOut[H <: HList, HL <: Nat](hList: KvpHListValue[ALG, H, HL])
      : TableCollection => Either[InvalidStructureError, TableCollection] =
      tableCollection => {
        val collectionWithColumns =
          columnCollectionInterpreter.fromKvpHList(hList.kvpHList)(tableCollection)
        Right(collectionWithColumns)
      }

    override protected def coproductToOut[C <: Coproduct](coproduct: CoproductCollection[ALG, C])
      : TableCollection => Either[InvalidStructureError, TableCollection] =
      _ => Left(InvalidStructureError("Coproduct not supported at the table level."))

    override protected def switchEncoding[A: Manifest, H <: HList, N <: Nat](
      hList: SwitchEncoding[ALG, H, N, A])
      : TableCollection => Either[InvalidStructureError, TableCollection] =
      tableCollection => {
        val collectionWithColumns =
          columnCollectionInterpreter.fromKvpHList(hList.from).apply(tableCollection)
        Right(collectionWithColumns)
      }

    override protected def coproductConvertToOut[C <: Coproduct, A: Manifest](
      cc: CoproductSwitch[ALG, C, A])
      : TableCollection => Either[InvalidStructureError, TableCollection] =
      _ => Left(InvalidStructureError("Coproduct not supported at the table level."))
  }

  val addColumnToWorkingTable = new ConcreteValueTemplate[
    ALG,
    (TableCollection, ColumnName, Option[Description]) => TableCollection] {
    override protected def optionalToOut[B: Manifest](opt: OptionalValue[ALG, B])
      : (TableCollection, ColumnName, Option[Description]) => TableCollection = {
      opt.valueDefinitionOp match {
        case Left(collection) => fromConcreteValue(collection)
        case Right(value) =>
          (collection: TableCollection, columnName: String, description: Option[String]) =>
            algInterpreter.columns(value)(collection, columnName, description)
      }
    }

    /**
      * If Either is a top-level entry point, then we will create two tables.  If it is not, we will
      * combine the two resulting tables, passing the names 'left' and 'right' as prefixes to the column names.
      * @param either The data describing the either.
      * @tparam A The left type.
      * @tparam B The right type
      * @return A function to create the tables.
      */
    override protected def eitherToOut[A: Manifest, B: Manifest](either: EitherData[ALG, A, B])
      : (TableCollection, ColumnName, Option[Description]) => TableCollection =
      (tableCollection, columnName, description) => {
        either.definitionA match {
          case Left(subCollection) =>
            fromConcreteValue(subCollection)
              .apply(
                tableCollection,
                DbUtil.camelToSnake(either.manifestOfLeft.getClass.getSimpleName),
                description)
          case Right(value) => {
            algInterpreter
              .columns(value)
              .apply(
                tableCollection,
                DbUtil.camelToSnake(either.manifestOfRight.getClass.getSimpleName),
                description)
          }
        }
      }

    override protected def listToOut[A: Manifest](list: ListData[ALG, A])
      : (TableCollection, ColumnName, Option[Description]) => TableCollection = {
      list.tDefinition match {
        case Left(col) => fromConcreteValue(col)
        case Right(alg) => {
          (
            tableCollection: TableCollection,
            columnName: ColumnName,
            description: Option[ColumnName]) =>
            algInterpreter.columns(alg)(tableCollection, columnName, description)
        }
      }
    }

    /**
      * Create a function, where it he column name is provided, we will create a new table collection.
      * @param hList
      * @tparam H
      * @tparam HL
      * @return
      */
    override protected def hListToOut[H <: HList, HL <: Nat](hList: KvpHListValue[ALG, H, HL])
      : (TableCollection, ColumnName, Option[Description]) => TableCollection = {

      (tableCollection: TableCollection, columnName: String, description: Option[String]) =>
        {
          val tc = tableCollection.startNewTable(columnName, description)
          columnCollectionInterpreter.fromKvpHList(hList.kvpHList)(tc)
        }
    }

    override protected def coproductToOut[C <: Coproduct](coproduct: CoproductCollection[ALG, C])
      : (TableCollection, ColumnName, Option[Description]) => TableCollection = {
      coproductInterpreter.fromKvpCoproduct(coproduct.kvpCoproduct)
    }

    override protected def switchEncoding[A: Manifest, H <: HList, N <: Nat](
      hList: SwitchEncoding[ALG, H, N, A])
      : (TableCollection, ColumnName, Option[Description]) => TableCollection =
      (tableCollection: TableCollection, columnName: String, description: Option[String]) => {
        val tc = tableCollection.startNewTable(columnName, description)
        columnCollectionInterpreter.fromKvpHList(hList.from)(tc)
      }

    override protected def coproductConvertToOut[C <: Coproduct, A: Manifest](
      cc: CoproductSwitch[ALG, C, A])
      : (TableCollection, ColumnName, Option[Description]) => TableCollection =
      coproductInterpreter.fromKvpCoproduct(cc.from)
  }

  val coproductInterpreter = new IdealCoproductInterpreter[ALG] {
    override val algInterpreter: IdealValue[ALG] = self.algInterpreter
    override def fromCollection[A: Manifest](kvpCollection: ConcreteValue[ALG, A])
      : (TableCollection, ColumnName, Option[Description]) => TableCollection =
      addColumnToWorkingTable.fromConcreteValue(kvpCollection)
  }
  val columnCollectionInterpreter = new IdealCollectionInterpreter[ALG] {
    override def fromConcreteValue[A: Manifest](
      concreteValue: ConcreteValue[ALG, A]): TableCollection => TableCollection =
      tableFromConcreteValue.fromConcreteValue(concreteValue)

    override val algInterpreter: IdealValue[ALG] = self.algInterpreter

  }

  def toIdeal[A: Manifest](
    schema: ConcreteValue[ALG, A],
    name: Option[String] = None,
    description: Option[String] = None): Either[InvalidStructureError, TableCollection] = {
    val tableName = name.getOrElse(DbUtil.camelToSnake(schema.manifestOfA.getClass.getSimpleName))
    val tableCol = TableCollection.init(tableName, description)
    tableFromConcreteValue.fromConcreteValue(schema).apply(tableCol)
  }

}
