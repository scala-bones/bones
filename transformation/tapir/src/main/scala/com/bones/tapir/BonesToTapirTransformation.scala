package com.bones.tapir

import com.bones.data.{
  EitherData,
  HigherOrderValue,
  KeyDefinition,
  KvpCoNil,
  KvpCollection,
  KvpCollectionValue,
  KvpCoproduct,
  KvpCoproductCollectionHead,
  KvpHListCollectionHead,
  KvpNil,
  KvpSingleValueHead,
  KvpWrappedCoproduct,
  KvpWrappedHList,
  ListData,
  OptionalValue
}
import shapeless.{:+:, ::, CNil, Coproduct, HList, Nat}
import sttp.tapir.SchemaType.{SArray, SCoproduct, SObjectInfo, SProduct}
import sttp.tapir.{FieldName, Schema}

trait BonesToTapirTransformation[ALG[_]] {

  val encoder: TapirValueTransformation[ALG]

  def kvpToSchemaList[A](kvp: KvpCollection[String, ALG, A]): Schema[List[A]] = {
    val fields = fromKvpCollection(kvp)
    val name = kvp.typeNameOfA.updated(0, kvp.typeNameOfA.charAt(0).toLower)
    val base = Schema(SProduct(SObjectInfo(name), fields), false, None, None, false)
    Schema(SArray(base))
  }

  def kvpToSchema[A](kvp: KvpCollection[String, ALG, A]): Schema[A] = {
    val fields = fromKvpCollection(kvp)
    val name = kvp.typeNameOfA.updated(0, kvp.typeNameOfA.charAt(0).toLower)
    Schema(SProduct(SObjectInfo(name), fields), false, None, None, false)
  }

  def fromKvpCollection[A](
    kvpCollection: KvpCollection[String, ALG, A]): List[(FieldName, Schema[_])] = {
    kvpCollection match {
      case kvp: KvpWrappedHList[String, ALG, a, h, n] @unchecked  => kvpWrappedHList(kvp)
      case kvp: KvpWrappedCoproduct[String, ALG, a, c] @unchecked => kvpWrappedCoproduct(kvp)
      case kvp: KvpCoNil[String, ALG]                             => kvpCoNil(kvp)
      case kvp: KvpCoproductCollectionHead[String, ALG, a, c, o] =>
        kvpCoproductCollectionHead[a, c, o](kvp)
      case kvp: KvpSingleValueHead[String, ALG, A, t, tl, ht] @unchecked =>
        kvpSingleValueHead[A, t, tl, ht](kvp)
      case kvp: KvpHListCollectionHead[String, ALG, ho, no, h, hl, t, tl] @unchecked =>
        kvpHListCollectionHead(kvp)
      case kvp: KvpNil[String, ALG] => kvpNil(kvp)
    }
  }

  def kvpWrappedHList[A, H <: HList, HL <: Nat](
    wrappedHList: KvpWrappedHList[String, ALG, A, H, HL]
  ): List[(FieldName, Schema[_])] = {
    fromKvpCollection(wrappedHList.wrappedEncoding)
  }

  def kvpWrappedCoproduct[A, C <: Coproduct](
    wrappedCoproduct: KvpWrappedCoproduct[String, ALG, A, C]
  ): List[(FieldName, Schema[_])] = {
    fromKvpCollection(wrappedCoproduct.wrappedEncoding)
  }

  def kvpHListCollectionHead[HO <: HList, NO <: Nat, H <: HList, HL <: Nat, T <: HList, TL <: Nat](
    kvp: KvpHListCollectionHead[String, ALG, HO, NO, H, HL, T, TL]
  ): List[(FieldName, Schema[_])] = {
    val head = fromKvpCollection(kvp.head)
    val tail = fromKvpCollection(kvp.tail)
    head ::: tail
  }

  def kvpNil(kvp: KvpNil[String, ALG]): List[(FieldName, Schema[_])] = List.empty

  def kvpSingleValueHead[H, T <: HList, TL <: Nat, O <: H :: T](
    kvp: KvpSingleValueHead[String, ALG, H, T, TL, O]
  ): List[(FieldName, Schema[_])] = {
    val head: List[(FieldName, Schema[_])] = kvp.head match {
      case Left(value) => List(keyDefinitionSchema(value))
      case Right(collection) =>
        fromKvpCollection(collection)
    }
    val tail = fromKvpCollection(kvp.tail)
    head ::: tail
  }

  def kvpCoNil(kvpCoNil: KvpCoNil[String, ALG]): List[(FieldName, Schema[_])] =
    List.empty

  def kvpCoproductCollectionHead[A, C <: Coproduct, O <: A :+: C](
    kvpCoproductCollectionHead: KvpCoproductCollectionHead[String, ALG, A, C, O]
  ): List[(FieldName, Schema[_])] = {
    val head = fromKvpCollection(kvpCoproductCollectionHead.kvpCollection)
    val tail = fromKvpCoproduct(kvpCoproductCollectionHead.kvpTail)
    val coproductSchemas = head.map(_._2) ::: tail.map(_._2)
    val schema = Schema(
      SCoproduct(SObjectInfo(kvpCoproductCollectionHead.typeNameOfA), coproductSchemas, None),
      false,
      None,
      None,
      false)
    List((FieldName(kvpCoproductCollectionHead.typeNameOfA), schema))
  }

  def fromKvpCoproduct[C <: Coproduct](
    value: KvpCoproduct[String, ALG, C]): List[(FieldName, Schema[_])] = {

    value match {
      case KvpCoNil() => List.empty
      case KvpCoproductCollectionHead(kvpCollection, _, kvpTail) => {
        val head = fromKvpCollection(kvpCollection)
        val tail = fromKvpCoproduct(kvpTail)
        head ::: tail
      }
    }

  }

  def keyDefinitionSchema[A](
    keyDefinition: KeyDefinition[String, ALG, A]): (FieldName, Schema[_]) = {
    val schema = determineValueDefinition(keyDefinition.dataDefinition)
    (FieldName(keyDefinition.key), schema)
  }

  def determineValueDefinition[A](
    dataDefinition: Either[HigherOrderValue[String, ALG, A], ALG[A]]
  ): Schema[_] = {
    dataDefinition match {
      case Left(kvp) => {
        primitiveSchema(kvp)
      }
      case Right(cov) => {
        val (schemaType, description, example) = encoder.toSchemaType(cov, None, None)
        Schema(schemaType, false, Some(description), Some(example), false)
      }
    }
  }

  def primitiveSchema[A](hov: HigherOrderValue[String, ALG, A]): Schema[_] = {
    hov match {
      case ed: EitherData[String, ALG, l, r] =>
        val left = determineValueDefinition(ed.definitionA)
        val right = determineValueDefinition(ed.definitionB)
        Schema(
          SCoproduct(SObjectInfo(hov.typeName), List(left, right), None),
          false,
          None,
          None,
          false)
      case ld: ListData[String, ALG, t] =>
        determineValueDefinition(ld.tDefinition).asArrayElement
      case od: OptionalValue[String, ALG, b] =>
        determineValueDefinition(od.valueDefinitionOp).asOptional
      case kvp: KvpCollectionValue[String, ALG, A] =>
        val fields = fromKvpCollection(kvp.kvpCollection)
        val schemaType = SProduct(SObjectInfo(kvp.typeName), fields)
        Schema(schemaType, false, None, None, false)
    }
  }

}
