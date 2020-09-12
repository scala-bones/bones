package com.bones.jdbc.ideal

import com.bones.data.KvpCollection.headManifest
import com.bones.data.template.KvpCollectionMatch
import com.bones.data.{ConcreteValueTemplate, _}
import com.bones.jdbc.DbUtil
import com.bones.jdbc.DbUtil.camelToSnake
import shapeless.{::, Coproduct, HList, Nat}

trait Ideal[ALG[_]] extends KvpCollectionMatch[ALG, TableCollection => TableCollection] { self =>

  def algInterpreter: IdealValue[ALG]

  val tableFromConcreteValue = new ConcreteValueTemplate[
    ALG,
    (TableCollection, ColumnName, Option[Description]) => TableCollection] {
    override protected def optionalToOut[B: Manifest](opt: OptionalValue[ALG, B])
      : (TableCollection, ColumnName, Option[Description]) => TableCollection = {
      determineValueDefinition(opt.valueDefinitionOp)
    }

    override protected def eitherToOut[A: Manifest, B: Manifest](either: EitherData[ALG, A, B])
      : (TableCollection, ColumnName, Option[Description]) => TableCollection = {
      val leftF = determineValueDefinition(either.definitionA)
      val leftName = camelToSnake(either.manifestOfLeft.runtimeClass.getSimpleName)
      val rightF = determineValueDefinition(either.definitionB)
      val rightName = camelToSnake(either.manifestOfRight.runtimeClass.getSimpleName)
      (tc, name, description) =>
        {
          val leftResult = leftF(tc, leftName, description)
          rightF(leftResult, rightName, description)
        }
    }

    override protected def listToOut[A: Manifest](list: ListData[ALG, A])
      : (TableCollection, ColumnName, Option[Description]) => TableCollection =
      determineValueDefinition(list.tDefinition)

    override protected def kvpCollectionToOut[A](hList: KvpCollectionValue[ALG, A])
      : (TableCollection, ColumnName, Option[Description]) => TableCollection =
      (tc, _, _) => fromKvpCollection(hList.kvpCollection)(tc)
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
      * @return
      */
    override protected def kvpCollectionToOut[A](hList: KvpCollectionValue[ALG, A])
      : (TableCollection, ColumnName, Option[Description]) => TableCollection = {

      (tableCollection: TableCollection, columnName: String, description: Option[String]) =>
        {
          val tc = tableCollection.startNewTable(columnName, description)
          fromKvpCollection(hList.kvpCollection)(tc)
        }
    }
  }

//  val columnCollectionInterpreter = new IdealCollectionInterpreter[ALG] {
//    override def fromConcreteValue[A: Manifest](
//      concreteValue: PrimitiveWrapperValue[ALG, A]): TableCollection => TableCollection =
//      tableFromConcreteValue.fromConcreteValue(concreteValue)
//    override val algInterpreter: IdealValue[ALG] = self.algInterpreter
//  }

  override def kvpNil(kvp: KvpNil[ALG]): TableCollection => TableCollection =
    identity

  override def kvpHListCollectionHead[
    HO <: HList,
    NO <: Nat,
    H <: HList,
    HL <: Nat,
    T <: HList,
    TL <: Nat](
    kvp: KvpHListCollectionHead[ALG, HO, NO, H, HL, T, TL]): TableCollection => TableCollection = {
    val head = fromKvpCollection(kvp.head)
    val tail = fromKvpCollection(kvp.tail)
    (tableCollection: TableCollection) =>
      {
        val headTableCollection = head(tableCollection)
        tail(headTableCollection)
      }
  }

  override def kvpWrappedHList[A, H <: HList, HL <: Nat](
    wrappedHList: KvpWrappedHList[ALG, A, H, HL]): TableCollection => TableCollection =
    fromKvpCollection(wrappedHList.wrappedEncoding)

  override def kvpWrappedCoproduct[A, C <: Coproduct](
    wrappedCoproduct: KvpWrappedCoproduct[ALG, A, C]): TableCollection => TableCollection =
    fromKvpCollection(wrappedCoproduct.wrappedEncoding)

  override def kvpSingleValueHead[H, T <: HList, TL <: Nat, O <: H :: T](
    kvp: KvpSingleValueHead[ALG, H, T, TL, O]): TableCollection => TableCollection = {
    val headResult = kvp.head match {
      case Left(keyDef) => { (tc: TableCollection) =>
        determineValueDefinition(keyDef.dataDefinition)(
          tc,
          camelToSnake(keyDef.key),
          keyDef.description)
      }
      case Right(kvpColl) => fromKvpCollection(kvpColl)
    }
    val tailResult = fromKvpCollection(kvp.tail)
    tc =>
      {
        val headTc = headResult(tc)
        tailResult(headTc)
      }
  }

  def determineValueDefinition[A](dataDefinition: Either[HigherOrderValue[ALG, A], ALG[A]])
    : (TableCollection, ColumnName, Option[Description]) => TableCollection = {
    dataDefinition match {
      case Left(primitiveWrapper) =>
        (tc, name, desc) =>
          tableFromConcreteValue.fromConcreteValue(primitiveWrapper)(primitiveWrapper.manifestOfA)(
            tc,
            name,
            desc)
      case Right(alg) => algInterpreter.columns(alg)
    }
  }

  override def kvpCoproduct[C <: Coproduct](
    value: KvpCoproduct[ALG, C]): TableCollection => TableCollection = {

    value match {
      case _: KvpCoNil[ALG] => identity
      case kvp: KvpCoproductCollectionHead[ALG, a, C, o] @unchecked => {
        val head = fromKvpCollection(kvp.kvpCollection)
        val tail = kvpCoproduct(kvp.kvpTail)
        tc =>
          {
            val headTc = head(tc)
            tail(headTc)
          }
      }
    }

  }

  def toIdeal[A: Manifest](
    schema: KvpCollection[ALG, A],
    name: Option[String] = None,
    description: Option[String] = None): TableCollection = {
    val tableName =
      DbUtil.camelToSnake(
        headManifest(schema).map(_.runtimeClass.getSimpleName).getOrElse("unknown"))
    val tableCol = TableCollection.init(tableName, description)
    fromKvpCollection(schema).apply(tableCol)
  }

}
