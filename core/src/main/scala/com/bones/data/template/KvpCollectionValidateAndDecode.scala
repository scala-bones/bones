package com.bones.data.template

import cats.data.NonEmptyList
import com.bones.{Path, Util}
import com.bones.data.Error.{ExtractionError, RequiredValue, SumTypeError}
import com.bones.data.{
  KeyDefinition,
  KvpCoNil,
  KvpCollection,
  KvpCoproduct,
  KvpCoproductCollectionHead,
  KvpHListCollectionHead,
  KvpNil,
  KvpSingleValueHead,
  KvpWrappedCoproduct,
  KvpWrappedHList
}
import com.bones.validation.ValidationUtil
import shapeless.{:+:, ::, Coproduct, HList, HNil, Inl, Inr, Nat}

trait KvpCollectionValidateAndDecode[ALG[_], IN] {

  type CoproductType
  type Path = List[String]
  type InputF[A] = (IN, Path) => Either[NonEmptyList[ExtractionError], A]

  def keyDefinition[A](
    value: KeyDefinition[ALG, A]): (IN, List[String]) => Either[NonEmptyList[ExtractionError], A]

  def coproductType(in: IN): Option[CoproductType]

  def fromKvpCollection[A](kvpCollection: KvpCollection[ALG, A]): InputF[A] = {
    kvpCollection match {
      case kvp: KvpWrappedHList[ALG, a, h, n] @unchecked  => kvpClass(kvp)
      case kvp: KvpWrappedCoproduct[ALG, a, c] @unchecked => kvpSuperclass(kvp)
      case kvp: KvpCoNil[ALG] =>
        (_, path) =>
          Left(NonEmptyList.one(RequiredValue(path, "Coproduct")))
      case kvp: KvpCoproductCollectionHead[ALG, a, c, o] =>
        kvpCoproductCollectionHead[a, c, o](kvp).asInstanceOf[InputF[A]]
      case kvp: KvpSingleValueHead[ALG, h, t, tl, a] @unchecked =>
        kvpSingleValueHead[h, t, tl, a](kvp).asInstanceOf[InputF[A]]
      case kvp: KvpHListCollectionHead[ALG, a, no, h, hl, t, tl] @unchecked =>
        kvpCollectionHead(kvp).asInstanceOf[InputF[A]]
      case kvp: KvpNil[ALG] => kvpNil(kvp).asInstanceOf[InputF[A]]
    }
  }

  def kvpClass[A, H <: HList, HL <: Nat](kvpClass: KvpWrappedHList[ALG, A, H, HL]): InputF[A] = {
    val wrappedF = fromKvpCollection(kvpClass.wrappedEncoding)
    (in, path) =>
      {
        val wrappedResult = wrappedF(in, path)
        wrappedResult.map(kvpClass.fHtoA)
      }
  }

  def kvpSuperclass[A, C <: Coproduct](kvpSuperclass: KvpWrappedCoproduct[ALG, A, C]): InputF[A] = {
    val wrappedF = fromKvpCollection(kvpSuperclass.wrappedEncoding)
    (in, path) =>
      {
        val wrappedResult = wrappedF(in, path)
        wrappedResult.map(kvpSuperclass.fCtoA)
      }
  }
  def kvpCollectionHead[HO <: HList, NO <: Nat, H <: HList, HL <: Nat, T <: HList, TL <: Nat](
    kvp: KvpHListCollectionHead[ALG, HO, NO, H, HL, T, TL]): InputF[HO] = {
    val headF = fromKvpCollection(kvp.head)
    val tailF = fromKvpCollection(kvp.tail)
    (in, path) =>
      {
        val headResult = headF(in, path)
        val tailResult = tailF(in, path)

        val combinedResult = Util.eitherMap2(headResult, tailResult) { (h, t) =>
          {
            kvp.prepend(h, t)
          }
        }
        for {
          cr <- combinedResult
          x <- ValidationUtil.validate(kvp.validations)(cr, path)
        } yield x

      }
  }

  def kvpNil(kvp: KvpNil[ALG]): InputF[HNil] = (_, _) => Right(HNil)

  def kvpSingleValueHead[H, T <: HList, TL <: Nat, O <: H :: T](
    kvp: KvpSingleValueHead[ALG, H, T, TL, O]): InputF[O] = {

    val headF = kvp.head match {
      case Left(value)       => keyDefinition(value)
      case Right(collection) => fromKvpCollection(collection)
    }

    val tailF = fromKvpCollection(kvp.tail)

    (in, path) =>
      {
        val headPath = kvp.head match {
          case Left(value) => path :+ value.key
          case Right(_)    => path
        }

        val headResult = headF(in, headPath)

        val tailResult = tailF(in, path)

        Util
          .eitherMap2(headResult, tailResult)((l1, l2) => {
            kvp.isHCons.cons(l1, l2)
          })
          .flatMap { l =>
            ValidationUtil.validate(kvp.validations)(l, path)
          }
      }
  }

  def kvpCoproductCollectionHead[A, C <: Coproduct, O <: A :+: C](
    headCoproduct: KvpCoproductCollectionHead[ALG, A, C, O]): InputF[O] = {

    def nestedKvpCoproduct[C2 <: Coproduct](co: KvpCoproduct[ALG, C2])
      : (IN, Path, CoproductType) => Either[NonEmptyList[ExtractionError], C2] =
      co match {
        case _: KvpCoNil[_] =>
          (_: IN, path: Path, coType: CoproductType) =>
            Left(NonEmptyList.one(SumTypeError(path, s"Unexpected type value: ${coType}")))
        case co: KvpCoproductCollectionHead[ALG, a, r, o] @unchecked => {
          val fValue = fromKvpCollection[a](co.kvpCollection)
          val fTail = nestedKvpCoproduct[r](co.kvpTail)
          (in, path, coType) =>
            {
              if (coType == co.manifestOfHead.runtimeClass.getSimpleName)
                fValue(in, path).map(Inl(_).asInstanceOf[C2])
              else fTail(in, path, coType).map(Inr(_).asInstanceOf[C2])
            }
        }
      }

    val nextedF = nestedKvpCoproduct(headCoproduct)
    (in, path) =>
      {
        coproductType(in) match {
          case None =>
            Left(NonEmptyList.one(RequiredValue(path, s"${headCoproduct.manifestOfHead}")))
          case Some(typeString) => nextedF(in, path, typeString)
        }
      }
  }
}
