package com.bones.jdbc.update

import com.bones.data.{
  EitherData,
  KvpCoNil,
  KvpCollectionValue,
  KvpCoproduct,
  KvpCoproductCollectionHead,
  KvpHListCollectionHead,
  KvpNil,
  KvpSingleValueHead,
  KvpWrappedCoproduct,
  KvpWrappedHList,
  ListData,
  OptionalValue,
  HigherOrderValue
}
import com.bones.data.template.KvpCollectionFunctor
import shapeless.{::, CNil, Coproduct, HList, HNil, Inl, Inr, Nat}

trait JdbcStatementInterpreter[ALG[_]]
    extends KvpCollectionFunctor[String, ALG, Lambda[A => Index => JdbcColumnStatement[A]]] {

  def customDbUpdateInterpreter: UpdateStatementValue[ALG]

  override def kvpNil(kvp: KvpNil[String, ALG]): Index => JdbcColumnStatement[HNil] =
    i => JdbcColumnStatement(i, List.empty, (h: HNil) => List.empty)

  override def kvpWrappedHList[A, H <: HList, HL <: Nat](
    wrappedHList: KvpWrappedHList[String, ALG, A, H, HL]): Index => JdbcColumnStatement[A] = {
    val wrappedF = fromKvpCollection(wrappedHList.wrappedEncoding)
    i =>
      {
        val wrappedDefResult = wrappedF(i)
        val predicateA = (a: A) => wrappedDefResult.predicates(wrappedHList.fAtoH(a))
        JdbcColumnStatement[A](
          wrappedDefResult.lastIndex,
          wrappedDefResult.assignmentStatements,
          predicateA)
      }
  }

  override def kvpWrappedCoproduct[A, C <: Coproduct](
    wrappedCoproduct: KvpWrappedCoproduct[String, ALG, A, C]): Index => JdbcColumnStatement[A] = {
    val wrappedF = fromKvpCollection(wrappedCoproduct.wrappedEncoding)
    (i: Index) =>
      {
        val wrappedResult = wrappedF(i)
        val predicates = (a: A) => wrappedResult.predicates(wrappedCoproduct.fAtoC(a))
        JdbcColumnStatement(wrappedResult.lastIndex, wrappedResult.assignmentStatements, predicates)
      }
  }

  override def kvpSingleValueHead[H, T <: HList, TL <: Nat, O <: H :: T](
    kvp: KvpSingleValueHead[String, ALG, H, T, TL, O]): Index => JdbcColumnStatement[O] = {
    val headF = kvp.head match {
      case Left(kd) => {
        val f = determineValueDefinition(kd.dataDefinition)
        (i: Index) =>
          f.apply(i, kd.key)
      }
      case Right(kvpCollection) => {
        fromKvpCollection(kvpCollection)
      }
    }
    val tailF = fromKvpCollection(kvp.tail)

    (i: Index) =>
      {
        val headResult = headF(i)
        val tailResult = tailF(headResult.lastIndex + 1)
        implicit val hCons = kvp.isHCons
        val f: O => List[SetValue] = (o: O) => {
          val headSetValues = headResult.predicates(o.head)
          val tailSetValue = tailResult.predicates(o.tail)
          headSetValues ::: tailSetValue
        }
        JdbcColumnStatement(
          tailResult.lastIndex,
          headResult.assignmentStatements ::: tailResult.assignmentStatements,
          f)
      }
  }

  override def kvpHListCollectionHead[
    HO <: HList,
    NO <: Nat,
    H <: HList,
    HL <: Nat,
    T <: HList,
    TL <: Nat](kvp: KvpHListCollectionHead[String, ALG, HO, NO, H, HL, T, TL])
    : Index => JdbcColumnStatement[HO] = {
    val headF = fromKvpCollection(kvp.head)
    val tailF = fromKvpCollection(kvp.tail)
    (i: Index) =>
      {
        val headResult = headF(i)
        val tailResult = tailF(headResult.lastIndex + 1)
        val f: HO => List[SetValue] = (ho: HO) => {
          val (h, t) = kvp.split(ho)
          val headSetValues = headResult.predicates(h)
          val tailSetValue = tailResult.predicates(t)
          headSetValues ::: tailSetValue
        }
        JdbcColumnStatement(
          tailResult.lastIndex,
          headResult.assignmentStatements ::: tailResult.assignmentStatements,
          f)
      }
  }

  def determineValueDefinition[A](
    valueDef: Either[HigherOrderValue[String, ALG, A], ALG[A]]
  ): (Index, Key) => JdbcColumnStatement[A] =
    valueDef match {
      case Left(kvp)  => valueDefinition[A](kvp)
      case Right(alg) => customDbUpdateInterpreter.definitionResult(alg)
    }

  def valueDefinition[A](
    fgo: HigherOrderValue[String, ALG, A]): (Index, Key) => JdbcColumnStatement[A] =
    fgo match {
      case op: OptionalValue[String, ALG, b] @unchecked =>
        val valueF = determineValueDefinition(op.valueDefinitionOp)
        (i, k) =>
          val ops = valueF.apply(i, k)

          val f: A => List[SetValue] = (a: A) => {
            a match {
              case Some(b) =>
                ops.predicates(b)
              case None => {
                // Instead of calling the sub statements, we set them all to null
                ops.assignmentStatements.map(_._2)
              }
            }
          }
          ops.copy(predicates = f)
      case _: ListData[String, ALG, t] @unchecked      => ???
      case _: EitherData[String, ALG, a, b] @unchecked => ???
      case kvp: KvpCollectionValue[String, ALG, a] @unchecked => { (i: Index, _: Key) =>
        fromKvpCollection(kvp.kvpCollection)(i).asInstanceOf[JdbcColumnStatement[A]]
      }
    }

  override def kvpCoproduct[C <: Coproduct](
    value: KvpCoproduct[String, ALG, C]): Index => JdbcColumnStatement[C] = {
    value match {
      case _: KvpCoNil[String, ALG] =>
        i =>
          JdbcColumnStatement[CNil](i, List.empty, _ => List.empty)
      case kvp: KvpCoproductCollectionHead[String, ALG, a, c, C] => {
        val headF = fromKvpCollection(kvp.kvpCollection)
        val tailF = kvpCoproduct(kvp.kvpTail)
        i =>
          {
            val headResult = headF(i)
            val tailResult = tailF(headResult.lastIndex)
            val predicate: C => List[SetValue] = o => {
              o match {
                case Inl(aValue) => headResult.predicates(aValue.asInstanceOf[a])
                case Inr(cValue) => tailResult.predicates(cValue.asInstanceOf[c])
              }
            }
            JdbcColumnStatement(
              tailResult.lastIndex,
              headResult.assignmentStatements ::: tailResult.assignmentStatements,
              predicate)
          }
      }
    }
  }
}
