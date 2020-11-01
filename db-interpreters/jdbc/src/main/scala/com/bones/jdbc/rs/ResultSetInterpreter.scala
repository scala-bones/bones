package com.bones.jdbc.rs

import java.sql.ResultSet

import cats.data.NonEmptyList
import com.bones.Util
import com.bones.Util.{NullValue, NullableResult, eitherMap2Nullable}
import com.bones.data.Error.{ExtractionErrors, RequiredValue}
import com.bones.data.KeyDefinition.CoproductDataDefinition
import com.bones.data._
import com.bones.data.template.KvpCollectionFunctor
import com.bones.jdbc.DbUtil.camelToSnake
import com.bones.jdbc.FindInterpreter.{FieldName, Path}
import shapeless.{::, Coproduct, HList, HNil, Inl, Inr, Nat}

/** Responsible for converting a result set into the result type */
trait ResultSetInterpreter[ALG[_]]
    extends KvpCollectionFunctor[
      ALG,
      Lambda[A => Path => ResultSet => Either[ExtractionErrors, NullableResult[A]]]] {

  val customInterpreter: ResultSetValue[ALG]

  def generateResultSet[A](
    collection: KvpCollection[ALG, A]): Path => ResultSet => Either[ExtractionErrors, A] =
    path =>
      resultSet => {
        val nullable = fromKvpCollection(collection)(path)(resultSet)
        nullable.flatMap {
          case Left(nulls) =>
            Left(nulls.map(n => RequiredValue(n.path :+ n.fieldName, n.typeName)))
          case Right(v) => Right(v)
        }
    }

  override def kvpNil(
    kvp: KvpNil[ALG]): Path => ResultSet => Either[ExtractionErrors, NullableResult[HNil]] =
    (_: Path) => (_: ResultSet) => Right(Right(HNil))

  override def kvpSingleValueHead[H, T <: HList, TL <: Nat, O <: H :: T](
    kvp: KvpSingleValueHead[ALG, H, T, TL, O])
    : Path => ResultSet => Either[ExtractionErrors, NullableResult[O]] = { path =>
    {
      val rsToHead: ResultSet => Either[ExtractionErrors, NullableResult[H]] =
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
          val headResult = rsToHead(rs)
          val tailResult = rsToTail(rs)
          Util.eitherMap2Nullable(headResult, tailResult)((l1: H, l2: T) => {
            kvp.isHCons.cons(l1, l2)
          })
        }
    }
  }

  override def kvpWrappedHList[A, H <: HList, HL <: Nat](
    wrappedHList: KvpWrappedHList[ALG, A, H, HL])
    : Path => ResultSet => Either[ExtractionErrors, NullableResult[A]] = {
    val f = fromKvpCollection(wrappedHList.wrappedEncoding)
    (path: Path) => (rs: ResultSet) =>
      f(path)(rs).map(nh => nh.map(wrappedHList.fHtoA))
  }

  override def kvpWrappedCoproduct[A, C <: Coproduct](
    wrappedCoproduct: KvpWrappedCoproduct[ALG, A, C])
    : Path => ResultSet => Either[ExtractionErrors, NullableResult[A]] = {
    val f = fromKvpCollection(wrappedCoproduct.wrappedEncoding)
    (path: Path) => (rs: ResultSet) =>
      f(path)(rs).map(cn => cn.map(wrappedCoproduct.fCtoA))

  }

  override def kvpHListCollectionHead[
    HO <: HList,
    NO <: Nat,
    H <: HList,
    HL <: Nat,
    T <: HList,
    TL <: Nat](kvp: KvpHListCollectionHead[ALG, HO, NO, H, HL, T, TL])
    : Path => ResultSet => Either[ExtractionErrors, NullableResult[HO]] = {
    val fHead = fromKvpCollection(kvp.head)
    val fTail = fromKvpCollection(kvp.tail)
    (path: Path) => (rs: ResultSet) =>
      {
        val headValue = fHead(path)(rs)
        val tailValue = fTail(path)(rs)

        eitherMap2Nullable(headValue, tailValue)((l1: H, l2: T) => {
          kvp.prepend(l1, l2)
        })
      }
  }

  def determineValueDefinition[A](coproduct: CoproductDataDefinition[ALG, A])
    : (Path, FieldName) => ResultSet => Either[ExtractionErrors, NullableResult[A]] =
    coproduct match {
      case Left(kvp)  => valueDefinition(kvp)
      case Right(alg) => customInterpreter.resultSet(alg)
    }

  def valueDefinition[A](fgo: HigherOrderValue[ALG, A])
    : (Path, FieldName) => ResultSet => Either[ExtractionErrors, NullableResult[A]] =
    fgo match {
      case op: OptionalValue[ALG, a] @unchecked =>
        (path, fieldName) =>
          val child =
            determineValueDefinition(op.valueDefinitionOp)(path, fieldName)
          rs =>
            child(rs)
              .map {
                case Left(_)  => Right(None)
                case Right(v) => Right(Some(v))
              }
              .map(_.asInstanceOf[NullableResult[Option[a]]])
      case ld: ListData[ALG, t] @unchecked => ???
      case ed: EitherData[ALG, a, b] @unchecked =>
        (path, fieldName) => rs =>
          {
            val leftField = determineValueDefinition(ed.definitionA)(path, "left_" + fieldName)(rs)
            leftField
              .flatMap { nullable =>
                {
                  nullable match {
                    case Left(leftNullValue) => {
                      determineValueDefinition(ed.definitionB)(path, "right_" + fieldName)(rs) match {
                        case Left(err) => Left(err)
                        case Right(v) => {
                          v match {
                            case Left(rightNullValue) =>
                              Right(Left(leftNullValue ::: rightNullValue))
                            case Right(value) =>
                              Right(Right(value))
                          }
                        }
                      }
                    }
                    case Right(v) =>
                      Right(Right(v))
                  }
                }
              }
              .map(_.asInstanceOf[NullableResult[Either[a, b]]])
          }

      case kvp: KvpCollectionValue[ALG, a] @unchecked =>
        val groupF = fromKvpCollection(kvp.kvpCollection)
        (path, _) => //Ignore fieldName here
          groupF(path).andThen(_.map(nv => nv.map(_.asInstanceOf[A])))
    }

  override def kvpCoproduct[C <: Coproduct](value: KvpCoproduct[ALG, C])
    : Path => ResultSet => Either[ExtractionErrors, NullableResult[C]] = { path =>
    { rs =>
      {
        val dtype = rs.getString("dtype")
        coproductWithDtype(value, rs, dtype, path)
      }
    }
  }

  private def coproductWithDtype[C <: Coproduct](
    kvp: KvpCoproduct[ALG, C],
    rs: ResultSet,
    dtype: String,
    path: List[String]): Either[ExtractionErrors, NullableResult[C]] = {
    kvp match {
      case _: KvpCoNil[ALG] =>
        Right(Left(NonEmptyList.one(NullValue(kvp.typeNameOfA, kvp.typeNameOfA, path))))
      case head: KvpCoproductCollectionHead[ALG, a, c, C] => {
        if (head.typeNameOfA.capitalize == dtype) {
          fromKvpCollection(head.kvpCollection)(path)(rs).map(nv => nv.map(Inl(_).asInstanceOf[C]))
        } else {
          coproductWithDtype(head.kvpTail, rs, dtype, path).map(nv =>
            nv.map(Inr(_).asInstanceOf[C]))
        }
      }
    }
  }

}
