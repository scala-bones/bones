package com.bones.jdbc.rs

import java.sql.{ResultSet, SQLException}

import cats.data.NonEmptyList
import com.bones.PrimitiveValue
import com.bones.Util
import com.bones.data.Error.{ExtractionError, RequiredValue, SystemError}
import com.bones.data.KeyDefinition.CoproductDataDefinition
import com.bones.data._
import com.bones.data.template.{KvpCollectionFunctor, KvpCollectionMatch}
import com.bones.jdbc.DbUtil.camelToSnake
import com.bones.jdbc.FindInterpreter.{FieldName, Path}
import shapeless.{Coproduct, HList, HNil, Nat, ::}

/** Responsible for converting a result set into the result type */
trait ResultSetInterpreter[ALG[_]]
    extends KvpCollectionFunctor[
      ALG,
      Lambda[A => Path => ResultSet => Either[NonEmptyList[ExtractionError], A]]] {

  val customInterpreter: ResultSetValue[ALG]

  override def kvpNil(
    kvp: KvpNil[ALG]): Path => ResultSet => Either[NonEmptyList[ExtractionError], HNil] =
    (_: Path) => (_: ResultSet) => Right(HNil)

  override def kvpSingleValueHead[H, T <: HList, TL <: Nat, O <: H :: T](
    kvp: KvpSingleValueHead[ALG, H, T, TL, O])
    : Path => ResultSet => Either[NonEmptyList[ExtractionError], O] = { path =>
    {
      val rsToHead: ResultSet => Either[NonEmptyList[ExtractionError], H] =
        kvp.head match {
          case Left(keyDef) => {
            val newPath = keyDef.key :: path
            determineValueDefinition(keyDef.dataDefinition)(newPath, camelToSnake(keyDef.key))
          }
          case Right(kvpCol) =>
            fromKvpCollection[H](kvpCol)(path)
        }

      val rsToTail = fromKvpCollection(kvp.tail)(path)
      rs =>
        {
          Util.eitherMap2(rsToHead(rs), rsToTail(rs))((l1: H, l2: T) => {
            kvp.isHCons.cons(l1, l2)
          })
        }
    }
  }

  override def kvpWrappedHList[A, H <: HList, HL <: Nat](
    wrappedHList: KvpWrappedHList[ALG, A, H, HL])
    : Path => ResultSet => Either[NonEmptyList[ExtractionError], A] = {
    val f = fromKvpCollection(wrappedHList.wrappedEncoding)
    (path: Path) => (rs: ResultSet) =>
      f(path)(rs).map(h => wrappedHList.fHtoA(h))
  }

  override def kvpWrappedCoproduct[A, C <: Coproduct](
    wrappedCoproduct: KvpWrappedCoproduct[ALG, A, C])
    : Path => ResultSet => Either[NonEmptyList[ExtractionError], A] = {
    val f = fromKvpCollection(wrappedCoproduct.wrappedEncoding)
    (path: Path) => (rs: ResultSet) =>
      f(path)(rs).map(c => wrappedCoproduct.fCtoA(c))

  }

  override def kvpHListCollectionHead[
    HO <: HList,
    NO <: Nat,
    H <: HList,
    HL <: Nat,
    T <: HList,
    TL <: Nat](kvp: KvpHListCollectionHead[ALG, HO, NO, H, HL, T, TL])
    : Path => ResultSet => Either[NonEmptyList[ExtractionError], HO] = {
    val fHead = fromKvpCollection(kvp.head)
    val fTail = fromKvpCollection(kvp.tail)
    (path: Path) => (rs: ResultSet) =>
      {
        val headValue = fHead(path)(rs)
        val tailValue = fTail(path)(rs)

        Util.eitherMap2(headValue, tailValue)((l1: H, l2: T) => {
          kvp.prepend(l1, l2)
        })
      }
  }

  def determineValueDefinition[A](coproduct: CoproductDataDefinition[ALG, A])
    : (Path, FieldName) => ResultSet => Either[NonEmptyList[ExtractionError], A] =
    coproduct match {
      case Left(kvp)  => valueDefinition(kvp)
      case Right(alg) => customInterpreter.resultSet(alg)
    }

  def generateResultSet[A](collection: KvpCollection[ALG, A])
    : Path => ResultSet => Either[NonEmptyList[ExtractionError], A] =
    path => fromKvpCollection(collection)(path)

  def valueDefinition[A](fgo: HigherOrderValue[ALG, A])
    : (Path, FieldName) => ResultSet => Either[NonEmptyList[ExtractionError], A] =
    fgo match {
      case op: OptionalValue[ALG, a] @unchecked =>
        (path, fieldName) =>
          val child =
            determineValueDefinition(op.valueDefinitionOp)(path, fieldName)
          rs =>
            {
              child(rs) match {
                case Left(errs) =>
                  if (errs.length == 1) errs.head match {
                    case RequiredValue(_, childOp) if childOp == op.valueDefinitionOp => Right(None)
                    case _                                                            => Left(errs)
                  } else {
                    Left[NonEmptyList[ExtractionError], Option[a]](errs)
                  }
                case Right(a) => Right(Some(a))
              }
            }: Either[NonEmptyList[ExtractionError], Option[a]]
      case ld: ListData[ALG, t] @unchecked => ???
      case ed: EitherData[ALG, a, b] @unchecked =>
        (path, fieldName) => rs =>
          {
            val leftField = determineValueDefinition(ed.definitionA)(path, "left_" + fieldName)(rs)
            val result = leftField match {
              //if the error is that the left is required, we will check the right.
              case Left(nel) =>
                if (nel.length == 1) {
                  nel.head match {
                    case RequiredValue(path, op) => { //TODO: Imrove this
                      determineValueDefinition(ed.definitionB)(path, "right_" + fieldName)(rs)
                        .map(Right(_))
                    }
                    case _ => Left(nel)
                  }
                } else {
                  Left(nel)
                }
              case Right(v) => Right(Left(v))
            }
            result
          }

      case kvp: KvpCollectionValue[ALG, a] @unchecked =>
        val groupF = fromKvpCollection(kvp.kvpCollection)
        (path, _) => //Ignore fieldName here
          groupF(path).andThen(_.map(_.asInstanceOf[A]))
    }

  override def kvpCoproduct[C <: Coproduct](
    value: KvpCoproduct[ALG, C]): Path => ResultSet => Either[NonEmptyList[ExtractionError], C] =
    throw new UnsupportedOperationException("currently not supported") //TODO support this, probably by making this a a typeclass
}
