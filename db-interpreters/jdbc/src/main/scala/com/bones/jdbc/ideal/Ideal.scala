package com.bones.jdbc.ideal

import com.bones.data.values.InvalidStructureError
import com.bones.data._
import com.bones.jdbc.DbUtil
import shapeless.{Coproduct, HList, Nat}

case class Ideal[ALG[_]](algInterpreter: IdealValue[ALG])
    extends KvpCollectionTemplate[
      ALG,
      (
        TableCollection,
        Option[ColumnName],
        Option[Description]) => Either[InvalidStructureError, TableCollection]] { self =>

  val coproductInterpreter = new IdealCoproductInterpreter[ALG] {
    override val algInterpreter: IdealValue[ALG] = self.algInterpreter
    override def fromCollection[A: Manifest](kvpCollection: KvpCollection[ALG, A]):
      (TableCollection, Option[ColumnName], Option[Description]) => Either[InvalidStructureError, TableCollection] = self.fromCollection(kvpCollection)
  }
  val hListInterpreter = new IdealHListInterpreter[ALG] {
    override def fromCollection[A: Manifest](kvpCollection: KvpCollection[ALG, A]): (TableCollection, Option[ColumnName], Option[Description]) => Either[InvalidStructureError, TableCollection] = self.fromCollection(kvpCollection)
    override val algInterpreter: IdealValue[ALG] = self.algInterpreter
  }

  def toIdeal[A:Manifest](schema: KvpCollection[ALG, A]): Either[InvalidStructureError, TableCollection] = {
    val name = DbUtil.camelToSnake(schema.manifestOfA.getClass.getSimpleName)
    val tc = TableCollection.init(name, None)
    fromCollection[A](schema).apply(tc, None, None)
  }

  override protected def optionalToOut[B:Manifest](opt: OptionalKvpValueDefinition[ALG, B]): (
    TableCollection,
    Option[ColumnName],
    Option[Description]) => Either[InvalidStructureError, TableCollection] = {
    opt.valueDefinitionOp match {
      case Left(collection) => fromCollection(collection)
      case Right(value) =>
        (collection: TableCollection, columnNameOpt: Option[String], description: Option[String]) =>
          {
            columnNameOpt
              .map(name => algInterpreter.columns(value)(collection, name, description))
              .toRight(InvalidStructureError("Column Name Not available.", List.empty))
          }
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
  override protected def eitherToOut[A:Manifest, B:Manifest](either: EitherData[ALG, A, B]): (
    TableCollection,
    Option[ColumnName],
    Option[Description]) => Either[InvalidStructureError, TableCollection] =
    (tableCollection, columnName, description) => {
      columnName match {
        case None =>
          either.definitionA match {
            case Left(subCollection) =>
              fromCollection(subCollection).apply(tableCollection, columnName, description)
            case Right(data) =>
              Left(InvalidStructureError(s"No column name for data: $data", List.empty))
          }
        case Some(name) =>
          either.definitionA match {
            case Left(subCollection) =>
              fromCollection(subCollection)
                .apply(tableCollection, Some(s"left_$name"), description)
            case Right(value) => {
              val tc =
                algInterpreter.columns(value).apply(tableCollection, s"left_$name", description)
              Right(tc)
            }
          }
      }
    }

  override protected def listToOut[A:Manifest](list: ListData[ALG, A]): (
    TableCollection,
    Option[ColumnName],
    Option[Description]) => Either[InvalidStructureError, TableCollection] = {
    list.tDefinition match {
      case Left(col) => fromCollection(col)
      case Right(alg) => {
        (
          tableCollection: TableCollection,
          columnName: Option[ColumnName],
          description: Option[ColumnName]) =>
          {
            columnName match {
              case Some(name) =>
                Right(algInterpreter.columns(alg)(tableCollection, name, description))
              case None =>
                Left(InvalidStructureError("collection must have a name"))
            }
          }
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
  override protected def hListToOut[H <: HList, HL <: Nat](hList: KvpHListValue[ALG, H, HL]): (
    TableCollection,
    Option[ColumnName],
    Option[Description]) => Either[InvalidStructureError, TableCollection] = {

    (tableCollection: TableCollection, columnName: Option[String], description: Option[String]) => {
      val tc = columnName match {
        case Some(name) => tableCollection.startNewTable(name, description)
        case None => tableCollection
      }
      hListInterpreter.fromKvpHList(hList.kvpHList)(tc)
    }
  }

  override protected def coproductToOut[C <: Coproduct](coproduct: KvpCoproductValue[ALG, C]): (
    TableCollection,
    Option[ColumnName],
    Option[Description]) => Either[InvalidStructureError, TableCollection] = {
    coproductInterpreter.fromKvpCoproduct(coproduct.kvpCoproduct)
  }

  override protected def hListConvertToOut[A: Manifest, H <: HList, N <: Nat](
    hList: HListConvert[ALG, H, N, A]): (
    TableCollection,
    Option[ColumnName],
    Option[Description]) => Either[InvalidStructureError, TableCollection] =     (tableCollection: TableCollection, columnName: Option[String], description: Option[String]) => {
    val tc = columnName match {
      case Some(name) => tableCollection.startNewTable(name, description)
      case None => tableCollection
    }
    hListInterpreter.fromKvpHList(hList.from)(tc)
  }


  override protected def coproductConvertToOut[C <: Coproduct, A:Manifest](
    cc: KvpCoproductConvert[ALG, C, A]): (
    TableCollection,
    Option[ColumnName],
    Option[Description]) => Either[InvalidStructureError, TableCollection] =
    coproductInterpreter.fromKvpCoproduct(cc.from)

}
