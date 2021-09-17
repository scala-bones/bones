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
import com.bones.interpreter.validator.Validator
import com.bones.validation.ValidationUtil
import shapeless.{:+:, ::, CNil, Coproduct, HList, HNil, Inl, Inr, Nat}

trait KvpCollectionValidateAndDecode[K, ALG[_], IN] {

  type CoproductType
  type Path[K] = List[K]
  type InputF[K, A] = (IN, Path[K]) => Either[ExtractionErrors[K], A]

  def keyDefinition[A](value: KeyDefinition[K, ALG, A]): Validator[K, ALG, A, IN]

  def coproductType(in: IN): Option[CoproductType]

  def fromKvpCollection[A](kvpCollection: KvpCollection[K, ALG, A]): Validator[K, ALG, A, IN] = {
    kvpCollection match {
      case kvp: KvpWrappedHList[K, ALG, a, h, n] @unchecked  => kvpClass(kvp)
      case kvp: KvpWrappedCoproduct[K, ALG, a, c] @unchecked => kvpSuperclass(kvp)
      case kvp: KvpCoNil[K, ALG] =>
        (_, path) => Left(List(RequiredValue(path, "Coproduct")))
      case kvp: KvpCoproductCollectionHead[K, ALG, a, c, o] =>
        kvpCoproductCollectionHead[a, c, o](kvp).asInstanceOf[Validator[K, ALG, A, IN]]
      case kvp: KvpSingleValueHead[K, ALG, h, t, tl, a] @unchecked =>
        kvpSingleValueHead[h, t, tl, a](kvp).asInstanceOf[Validator[K, ALG, A, IN]]
      case kvp: KvpHListCollectionHead[K, ALG, a, no, h, hl, t, tl] @unchecked =>
        kvpCollectionHead(kvp).asInstanceOf[Validator[K, ALG, A, IN]]
      case kvp: KvpNil[K, ALG] => kvpNil(kvp).asInstanceOf[Validator[K, ALG, A, IN]]
    }
  }

  def kvpClass[A, H <: HList, HL <: Nat](
    kvpClass: KvpWrappedHList[K, ALG, A, H, HL]
  ): Validator[K, ALG, A, IN] = {
    val wrappedF = fromKvpCollection(kvpClass.wrappedEncoding)
    (in, path) => {
      val wrappedResult = wrappedF.validateWithPath(in, path)
      wrappedResult.map(kvpClass.fHtoA)
    }
  }

  def kvpSuperclass[A, C <: Coproduct](
    kvpSuperclass: KvpWrappedCoproduct[K, ALG, A, C]
  ): Validator[K, ALG, A, IN] = {
    val wrappedF = fromKvpCollection(kvpSuperclass.wrappedEncoding)
    (in: IN, path: List[K]) => {
      val wrappedResult = wrappedF.validateWithPath(in, path)
      wrappedResult.map(kvpSuperclass.fCtoA)
    }
  }
  def kvpCollectionHead[HO <: HList, NO <: Nat, H <: HList, HL <: Nat, T <: HList, TL <: Nat](
    kvp: KvpHListCollectionHead[K, ALG, HO, NO, H, HL, T, TL]
  ): Validator[K, ALG, HO, IN] = {
    val headF = fromKvpCollection(kvp.head)
    val tailF = fromKvpCollection(kvp.tail)
    (in, path) => {
      val headResult = headF.validateWithPath(in, path)
      val tailResult = tailF.validateWithPath(in, path)

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

  def kvpNil(kvp: KvpNil[K, ALG]): Validator[K, ALG, HNil, IN] = (_, _) => Right(HNil)

  def kvpSingleValueHead[H, T <: HList, TL <: Nat, O <: H :: T](
    kvp: KvpSingleValueHead[K, ALG, H, T, TL, O]
  ): Validator[K, ALG, O, IN] = {

    val headF = kvp.head match {
      case Left(value)       => keyDefinition(value)
      case Right(collection) => fromKvpCollection(collection)
    }

    val tailF = fromKvpCollection(kvp.tail)

    (in, path) => {
      val headPath = kvp.head match {
        case Left(value) => path :+ value.key
        case Right(_)    => path
      }

      val headResult = headF.validateWithPath(in, headPath)

      val tailResult = tailF.validateWithPath(in, path)

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
    headCoproduct: KvpCoproductCollectionHead[K, ALG, A, C, O]
  ): Validator[K, ALG, O, IN] = {

    def nestedKvpCoproduct[C2 <: Coproduct](
      co: KvpCoproduct[K, ALG, C2]
    ): CoproductType => Validator[K, ALG, C2, IN] =
      co match {
        case _: KvpCoNil[K, ALG] @unchecked =>
          coType =>
            new Validator[K, ALG, CNil, IN] {
              override def validateWithPath(
                in: IN,
                path: List[K]
              ): Either[ExtractionErrors[K], CNil] =
                Left(List(SumTypeError(path, s"Unexpected type value: ${coType}")))
            }
        case co: KvpCoproductCollectionHead[K, ALG, a, r, o] @unchecked => {
          val fValue = fromKvpCollection[a](co.kvpCollection)
          val fTail = nestedKvpCoproduct[r](co.kvpTail)
          coType =>
            (in: IN, path: List[K]) => {
              if (coType == co.typeNameOfA)
                fValue
                  .validateWithPath(in, path)
                  .map(Inl(_))
                  .asInstanceOf[Either[ExtractionErrors[K], C2]]
              else
                fTail(coType)
                  .validateWithPath(in, path)
                  .map(Inr(_))
                  .asInstanceOf[Either[ExtractionErrors[K], C2]]
            }
        }
      }

    (in: IN, path: List[K]) => {
      coproductType(in) match {
        case None =>
          Left(List(RequiredValue(path, s"${headCoproduct.typeNameOfA}")))
        case Some(typeString) =>
          nestedKvpCoproduct(headCoproduct)(typeString).validateWithPath(in, path)
      }
    }

  }
}
