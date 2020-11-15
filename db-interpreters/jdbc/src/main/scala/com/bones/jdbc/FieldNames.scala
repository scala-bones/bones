package com.bones.jdbc

import java.util.{Calendar, TimeZone}

import com.bones.data._
import com.bones.data.template.KvpCollectionMatch
import com.bones.data.values.AnyAlg
import com.bones.jdbc.DbUtil.camelToSnake
import shapeless.{::, Coproduct, HList, Nat}

object FindInterpreter {

  val utcCalendar = Calendar.getInstance(TimeZone.getTimeZone("UTC"))
  type FieldName = String

}

object TableName {

  def getTableName[ALG[_], B](dc: KvpCollection[String, ALG, B]): String = dc match {
    case wrapped: WrappedEncoding[String, ALG, B] =>
      camelToSnake(wrapped.typeNameOfA)
    case _ => "Unknown"
  }
}

object FieldNames {
  trait CustomFieldNamesInterpreter[ALG[_]] {
    def fieldNames[A](alg: ALG[A]): List[String]
  }
}

trait FieldNames[ALG[_]] extends KvpCollectionMatch[String, ALG, List[String]] {

  def customFieldNamesInterpreter: FieldNames.CustomFieldNamesInterpreter[ALG]

  def fromCustomSchema[A](dc: KvpCollection[String, ALG, A]): List[String] = fromKvpCollection(dc)

  override def kvpNil(kvp: KvpNil[String, ALG]): List[String] = List.empty

  override def kvpHListCollectionHead[
    HO <: HList,
    NO <: Nat,
    H <: HList,
    HL <: Nat,
    T <: HList,
    TL <: Nat](kvp: KvpHListCollectionHead[String, ALG, HO, NO, H, HL, T, TL]): List[String] =
    fromKvpCollection(kvp.head) ::: fromKvpCollection(kvp.tail)

  override def kvpWrappedHList[A, H <: HList, HL <: Nat](
    wrappedHList: KvpWrappedHList[String, ALG, A, H, HL]): List[String] =
    fromKvpCollection(wrappedHList.wrappedEncoding)

  override def kvpSingleValueHead[H, T <: HList, TL <: Nat, O <: H :: T](
    kvp: KvpSingleValueHead[String, ALG, H, T, TL, O]): List[String] = {
    kvp.head match {
      case Left(keyDef)         => determineValueDefinition(keyDef.dataDefinition)
      case Right(kvpCollection) => fromKvpCollection(kvpCollection)
    }
  }

  override def kvpCoproduct[C <: Coproduct](value: KvpCoproduct[String, ALG, C]): List[String] = {
    value match {
      case _: KvpCoNil[String, ALG] => List.empty
      case pr: KvpCoproductCollectionHead[String, ALG, a, c, o] => {
        fromKvpCollection(pr.kvpCollection) ::: kvpCoproduct(pr.kvpTail)
      }
    }
  }

  override def kvpWrappedCoproduct[A, C <: Coproduct](
    wrappedCoproduct: KvpWrappedCoproduct[String, ALG, A, C]): List[String] =
    "dtype" :: fromKvpCollection(wrappedCoproduct.wrappedEncoding)

  def determineValueDefinition[A](
    valueDefinitionOp: Either[HigherOrderValue[String, ALG, A], AnyAlg[A]]): List[String] =
    valueDefinitionOp match {
      case Left(kvp) => valueDefinition(kvp)
      case Right(_)  => List.empty
    }

  def valueDefinition[A](
    fgo: HigherOrderValue[String, ALG, A]
  ): List[String] =
    fgo match {
      case op: OptionalValue[String, ALG, a] @unchecked =>
        determineValueDefinition(op.valueDefinitionOp)
      case _: ListData[String, ALG, t] @unchecked      => List.empty
      case _: EitherData[String, ALG, a, b] @unchecked => List.empty
      case kvp: KvpCollectionValue[String, ALG, a] @unchecked =>
        fromKvpCollection(kvp.kvpCollection)
    }

}
