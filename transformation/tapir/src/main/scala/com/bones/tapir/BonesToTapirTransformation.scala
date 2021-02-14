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
import shapeless.ops.hlist.{Mapped, Prepend}
import shapeless.{:+:, ::, CNil, Coproduct, HList, HNil, Nat}
import sttp.tapir.SchemaType.{SArray, SCoproduct, SObjectInfo, SProduct}
import sttp.tapir.{FieldName, Schema, SchemaType, Validator}

trait BonesToTapirTransformation[ALG[_]] {

  val encoder: TapirValueTransformation[ALG]

  def kvpToSchemaList[A](kvp: KvpCollection[String, ALG, A]): Schema[List[A]] = {
    val schema = fromKvpCollection(kvp)
    val name = kvp.typeNameOfA.updated(0, kvp.typeNameOfA.charAt(0).toLower)
    val newType = schema.schemaType match {
      case SProduct(info, fields) => SProduct(info.copy(fullName = name), fields)
      case SCoproduct(info, schemas, discriminator) =>
        SCoproduct(info.copy(fullName = name), schemas, discriminator)
      case x => throw new IllegalStateException(s"Unexpected type: ${x}")
    }
    val base = schema.copy(schemaType = newType)
    Schema(SArray(base))
  }

  def kvpToSchema[A](kvp: KvpCollection[String, ALG, A]): Schema[A] = {
    val schema = fromKvpCollection(kvp)
    val name = kvp.typeNameOfA.updated(0, kvp.typeNameOfA.charAt(0).toLower)
    val newType = schema.schemaType match {
      case SProduct(info, fields) => SProduct(info.copy(fullName = name), fields)
      case SCoproduct(info, schemas, discriminator) =>
        SCoproduct(info.copy(fullName = name), schemas, discriminator)
      case x => throw new IllegalStateException(s"Unexpected type: ${x}")
    }
    schema.copy(schemaType = newType)
  }

  def fromKvpCollection[A](kvpCollection: KvpCollection[String, ALG, A]): Schema[A] = {
    kvpCollection match {
      case kvp: KvpWrappedHList[String, ALG, a, h, n] @unchecked  => kvpWrappedHList(kvp)
      case kvp: KvpWrappedCoproduct[String, ALG, a, c] @unchecked => kvpWrappedCoproduct(kvp)
      case kvp: KvpCoNil[String, ALG]                             => kvpCoNil(kvp)
      case kvp: KvpCoproductCollectionHead[String, ALG, a, c, o] =>
        kvpCoproductCollectionHead[a, c, o](kvp).asInstanceOf[Schema[A]]
      case kvp: KvpSingleValueHead[String, ALG, A, t, tl, ht] @unchecked =>
        kvpSingleValueHead[A, t, tl, ht](kvp).asInstanceOf[Schema[A]]
      case kvp: KvpHListCollectionHead[String, ALG, ho, no, h, hl, t, tl] @unchecked =>
        kvpHListCollectionHead(kvp).asInstanceOf[Schema[A]]
      case kvp: KvpNil[String, ALG] => kvpNil(kvp).asInstanceOf[Schema[A]]
    }
  }

  def kvpWrappedHList[A, H <: HList, HL <: Nat](
    wrappedHList: KvpWrappedHList[String, ALG, A, H, HL]
  ): Schema[A] = {
    val schemaH = fromKvpCollection(wrappedHList.wrappedEncoding)
    val validatorA = schemaH.validator.contramap(wrappedHList.fAtoH)
    schemaH.copy(default = None, validator = validatorA)
  }

  def kvpWrappedCoproduct[A, C <: Coproduct](
    wrappedCoproduct: KvpWrappedCoproduct[String, ALG, A, C]
  ): Schema[A] = {
    val schemaC = fromKvpCollection(wrappedCoproduct.wrappedEncoding)
    val validatorA = schemaC.validator.contramap(wrappedCoproduct.fAtoC)
    schemaC.copy(default = None, validator = validatorA)
  }

  def kvpHListCollectionHead[HO <: HList, NO <: Nat, H <: HList, HL <: Nat, T <: HList, TL <: Nat](
    kvp: KvpHListCollectionHead[String, ALG, HO, NO, H, HL, T, TL]
  ): Schema[HO] = {
    val head = fromKvpCollection(kvp.head)
    val tail = fromKvpCollection(kvp.tail)
    combineSchemaHead(head, tail, kvp.prepend)
  }

  def combineSchemaHead[H <: HList, T <: HList, HO <: HList](
    s1: Schema[H],
    s2: Schema[T],
    prepend: Prepend.Aux[H, T, HO],
  ): Schema[HO] = {
    val newType = (s1.schemaType, s2.schemaType) match {
      case (SProduct(i1, f1), SProduct(i2, f2)) => {
        val sObjectInfo = SchemaType.SObjectInfo(
          i1.fullName,
          i1.typeParameterShortNames ::: i2.typeParameterShortNames)
        SProduct(sObjectInfo, f1 ++ f2)
      }
      case x => throw new IllegalStateException(s"Unexpected case: ${x}")
    }
    Schema[HO](newType)
  }

  def kvpNil(kvp: KvpNil[String, ALG]): Schema[HNil] =
    Schema(SchemaType.SProduct(SchemaType.SObjectInfo.Unit, List.empty))

  def kvpSingleValueHead[H, T <: HList, TL <: Nat, O <: H :: T](
    kvp: KvpSingleValueHead[String, ALG, H, T, TL, O]
  ): Schema[O] = {
    val head: Schema[H] = kvp.head match {
      case Left(value) => {
        Schema(SchemaType.SProduct(SchemaType.SObjectInfo.Unit, List(keyDefinitionSchema(value))))
      }
      case Right(collection) =>
        fromKvpCollection(collection)
    }
    val tail = fromKvpCollection(kvp.tail)
    combineSchema(head, tail)
  }

  def combineSchema[H, T <: HList, O <: H :: T](s1: Schema[H], s2: Schema[T]): Schema[O] = {
    val newType = (s1.schemaType, s2.schemaType) match {
      case (SProduct(i1, f1), SProduct(i2, f2)) => {
        val sObjectInfo = SchemaType.SObjectInfo(
          i1.fullName,
          i1.typeParameterShortNames ::: i2.typeParameterShortNames)
        SProduct(sObjectInfo, f1 ++ f2)
      }
      case x => throw new IllegalStateException(s"Unexpected case: ${x}")
    }
    Schema(newType)
  }

  def kvpCoNil(kvpCoNil: KvpCoNil[String, ALG]): Schema[CNil] =
    Schema(SCoproduct(SObjectInfo.Unit, List.empty, None))

  def kvpCoproductCollectionHead[A, C <: Coproduct, O <: A :+: C](
    kvpCoproductCollectionHead: KvpCoproductCollectionHead[String, ALG, A, C, O]
  ): Schema[C] = {
    val head = fromKvpCollection(kvpCoproductCollectionHead.kvpCollection)
    val tail = fromKvpCoproduct(kvpCoproductCollectionHead.kvpTail)
    val coproductSchemas = List(head, tail)
    Schema[C](
      SCoproduct(SObjectInfo(kvpCoproductCollectionHead.typeNameOfA), coproductSchemas, None),
      false,
      None,
      None,
      None,
      None,
      false)
  }

  def fromKvpCoproduct[C <: Coproduct](value: KvpCoproduct[String, ALG, C]): Schema[C] = {

    def rec[H <: Coproduct, T <: Coproduct](
      headSchema: Schema[H],
      recValue: KvpCoproduct[String, ALG, T]): Schema[T] = {

//      recValue match {
//        case KvpCoNil() => headSchema
//      }

      ???
    }

    value match {
      case KvpCoNil() => sys.error("Unreachable code")
      case KvpCoproductCollectionHead(kvpCollection, _, kvpTail) => {
        val head = fromKvpCollection(kvpCollection)
        val tail = fromKvpCoproduct(kvpTail)
        ???
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
        Schema(schemaType, false, Some(description), None, None, Some(example), false)
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
          None,
          None,
          false)
      case ld: ListData[String, ALG, t] =>
        determineValueDefinition(ld.tDefinition).asArray
      case od: OptionalValue[String, ALG, b] =>
        determineValueDefinition(od.valueDefinitionOp).asOption
      case kvp: KvpCollectionValue[String, ALG, A] =>
        val fields = fromKvpCollection(kvp.kvpCollection)
        val incorrect = Iterable(FieldName("incorrect") -> fields)
        val schemaType = SProduct(SObjectInfo(kvp.typeName), incorrect)
        Schema(schemaType, false, None, None, None, None, false)
    }
  }

}
