package com.bones.data.template

import com.bones.{Path, Util}
import com.bones.data.Error.{ExtractionError, ExtractionErrors, RequiredValue, SumTypeError}
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

trait KvpCollectionValidateAndDecode[K, ALG[_], IN] {

  type CoproductType
  type Path[K] = List[K]
  type InputF[K, A] = (IN, Path[K]) => Either[ExtractionErrors[K], A]

  def keyDefinition[A](
    value: KeyDefinition[K, ALG, A]): (IN, List[K]) => Either[ExtractionErrors[K], A]

  def coproductType(in: IN): Option[CoproductType]

  def fromKvpCollection[A](kvpCollection: KvpCollection[K, ALG, A]): InputF[K, A] = {
    kvpCollection match {
      case kvp: KvpWrappedHList[K, ALG, a, h, n] @unchecked  => kvpClass(kvp)
      case kvp: KvpWrappedCoproduct[K, ALG, a, c] @unchecked => kvpSuperclass(kvp)
      case kvp: KvpCoNil[K, ALG] =>
        (_, path) =>
          Left(List(RequiredValue(path, "Coproduct")))
      case kvp: KvpCoproductCollectionHead[K, ALG, a, c, o] =>
        kvpCoproductCollectionHead[a, c, o](kvp).asInstanceOf[InputF[K, A]]
      case kvp: KvpSingleValueHead[K, ALG, h, t, tl, a] @unchecked =>
        kvpSingleValueHead[h, t, tl, a](kvp).asInstanceOf[InputF[K, A]]
      case kvp: KvpHListCollectionHead[K, ALG, a, no, h, hl, t, tl] @unchecked =>
        kvpCollectionHead(kvp).asInstanceOf[InputF[K, A]]
      case kvp: KvpNil[K, ALG] => kvpNil(kvp).asInstanceOf[InputF[K, A]]
    }
  }

  def kvpClass[A, H <: HList, HL <: Nat](
    kvpClass: KvpWrappedHList[K, ALG, A, H, HL]): InputF[K, A] = {
    val wrappedF = fromKvpCollection(kvpClass.wrappedEncoding)
    (in, path) =>
      {
        val wrappedResult = wrappedF(in, path)
        wrappedResult.map(kvpClass.fHtoA)
      }
  }

  def kvpSuperclass[A, C <: Coproduct](
    kvpSuperclass: KvpWrappedCoproduct[K, ALG, A, C]): InputF[K, A] = {
    val wrappedF = fromKvpCollection(kvpSuperclass.wrappedEncoding)
    (in, path) =>
      {
        val wrappedResult = wrappedF(in, path)
        wrappedResult.map(kvpSuperclass.fCtoA)
      }
  }
  def kvpCollectionHead[HO <: HList, NO <: Nat, H <: HList, HL <: Nat, T <: HList, TL <: Nat](
    kvp: KvpHListCollectionHead[K, ALG, HO, NO, H, HL, T, TL]): InputF[K, HO] = {
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

  def kvpNil(kvp: KvpNil[K, ALG]): InputF[K, HNil] = (_, _) => Right(HNil)

  def kvpSingleValueHead[H, T <: HList, TL <: Nat, O <: H :: T](
    kvp: KvpSingleValueHead[K, ALG, H, T, TL, O]): InputF[K, O] = {

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
    headCoproduct: KvpCoproductCollectionHead[K, ALG, A, C, O]): InputF[K, O] = {

    def nestedKvpCoproduct[C2 <: Coproduct](co: KvpCoproduct[K, ALG, C2])
      : (IN, Path[K], CoproductType) => Either[ExtractionErrors[K], C2] =
      co match {
        case _: KvpCoNil[K, ALG] @unchecked =>
          (_: IN, path: Path[K], coType: CoproductType) =>
            Left(List(SumTypeError(path, s"Unexpected type value: ${coType}")))
        case co: KvpCoproductCollectionHead[K, ALG, a, r, o] @unchecked => {
          val fValue = fromKvpCollection[a](co.kvpCollection)
          val fTail = nestedKvpCoproduct[r](co.kvpTail)
          (in, path, coType) =>
            {
              if (coType == co.typeNameOfA)
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
            Left(List(RequiredValue(path, s"${headCoproduct.typeNameOfA}")))
          case Some(typeString) => nextedF(in, path, typeString)
        }
      }
  }
}
