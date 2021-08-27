package com.bones.jdbc.rs

import java.sql.ResultSet

import com.bones.Util.{OmittedValue, CanBeOmitted, eitherMap2Nullable}
import com.bones.data.Error.{ExtractionErrors, RequiredValue}
import com.bones.data.KeyDefinition.CoproductDataDefinition
import com.bones.data._
import com.bones.data.template.KvpCollectionFunctor
import com.bones.jdbc.DbUtil.camelToSnake
import com.bones.jdbc.FindInterpreter.FieldName
import com.bones.{Path, Util}
import shapeless.{::, Coproduct, HList, HNil, Inl, Inr, Nat}

/** Responsible for converting a result set into the result type */
trait ResultSetInterpreter[ALG[_]]
    extends KvpCollectionFunctor[String, ALG, Lambda[
      A => Path[String] => ResultSet => Either[ExtractionErrors[String], CanBeOmitted[String, A]]
    ]] {

  val customInterpreter: ResultSetValue[ALG]

  def generateResultSet[A](
    collection: KvpCollection[String, ALG, A]
  ): Path[String] => ResultSet => Either[ExtractionErrors[String], A] =
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
    kvp: KvpNil[String, ALG]
  ): Path[String] => ResultSet => Either[ExtractionErrors[String], CanBeOmitted[String, HNil]] =
    (_: Path[String]) => (_: ResultSet) => Right(Right(HNil))

  override def kvpSingleValueHead[H, T <: HList, TL <: Nat, O <: H :: T](
    kvp: KvpSingleValueHead[String, ALG, H, T, TL, O]
  ): Path[String] => ResultSet => Either[ExtractionErrors[String], CanBeOmitted[String, O]] = {
    path =>
      {
        val rsToHead: ResultSet => Either[ExtractionErrors[String], CanBeOmitted[String, H]] =
          kvp.head match {
            case Left(keyDef) => {
              val newPath = keyDef.key :: path
              determineValueDefinition(keyDef.dataDefinition)(newPath, camelToSnake(keyDef.key))
            }
            case Right(kvpCol) =>
              fromKvpCollection[H](kvpCol)(path)
          }

        val rsToTail = fromKvpCollection(kvp.tail)(path)
        rs => {
          val headResult = rsToHead(rs)
          val tailResult = rsToTail(rs)
          Util.eitherMap2Nullable(headResult, tailResult)((l1: H, l2: T) => {
            kvp.isHCons.cons(l1, l2)
          })
        }
      }
  }

  override def kvpWrappedHList[A, H <: HList, HL <: Nat](
    wrappedHList: KvpWrappedHList[String, ALG, A, H, HL]
  ): Path[String] => ResultSet => Either[ExtractionErrors[String], CanBeOmitted[String, A]] = {
    val f = fromKvpCollection(wrappedHList.wrappedEncoding)
    (path: Path[String]) => (rs: ResultSet) => f(path)(rs).map(nh => nh.map(wrappedHList.fHtoA))
  }

  override def kvpWrappedCoproduct[A, C <: Coproduct](
    wrappedCoproduct: KvpWrappedCoproduct[String, ALG, A, C]
  ): Path[String] => ResultSet => Either[ExtractionErrors[String], CanBeOmitted[String, A]] = {
    val f = fromKvpCollection(wrappedCoproduct.wrappedEncoding)
    (path: Path[String]) => (rs: ResultSet) => f(path)(rs).map(cn => cn.map(wrappedCoproduct.fCtoA))

  }

  override def kvpHListCollectionHead[
    HO <: HList,
    NO <: Nat,
    H <: HList,
    HL <: Nat,
    T <: HList,
    TL <: Nat
  ](
    kvp: KvpHListCollectionHead[String, ALG, HO, NO, H, HL, T, TL]
  ): Path[String] => ResultSet => Either[ExtractionErrors[String], CanBeOmitted[String, HO]] = {
    val fHead = fromKvpCollection(kvp.head)
    val fTail = fromKvpCollection(kvp.tail)
    (path: Path[String]) =>
      (rs: ResultSet) => {
        val headValue = fHead(path)(rs)
        val tailValue = fTail(path)(rs)

        eitherMap2Nullable(headValue, tailValue)((l1: H, l2: T) => {
          kvp.prepend(l1, l2)
        })
      }
  }

  def determineValueDefinition[A](coproduct: CoproductDataDefinition[String, ALG, A]): (
    Path[String],
    FieldName
  ) => ResultSet => Either[ExtractionErrors[String], CanBeOmitted[String, A]] =
    coproduct match {
      case Left(kvp)  => valueDefinition(kvp)
      case Right(alg) => customInterpreter.resultSet(alg)
    }

  def valueDefinition[A](fgo: HigherOrderValue[String, ALG, A]): (
    Path[String],
    FieldName
  ) => ResultSet => Either[ExtractionErrors[String], CanBeOmitted[String, A]] =
    fgo match {
      case op: OptionalValue[String, ALG, a] @unchecked =>
        (path, fieldName) =>
          val child =
            determineValueDefinition(op.valueDefinitionOp)(path, fieldName)
          rs =>
            child(rs)
              .map {
                case Left(_)  => Right(None)
                case Right(v) => Right(Some(v))
              }
              .map(_.asInstanceOf[CanBeOmitted[String, Option[a]]])
      case ld: ListData[String, ALG, t] @unchecked => ???
      case ed: EitherData[String, ALG, a, b] @unchecked =>
        (path, fieldName) =>
          rs => {
            val leftField = determineValueDefinition(ed.definitionA)(path, "left_" + fieldName)(rs)
            leftField
              .flatMap { nullable =>
                {
                  nullable match {
                    case Left(leftNullValue) => {
                      determineValueDefinition(ed.definitionB)(path, "right_" + fieldName)(
                        rs
                      ) match {
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
              .map(_.asInstanceOf[CanBeOmitted[String, Either[a, b]]])
          }

      case kvp: KvpCollectionValue[String, ALG, a] @unchecked =>
        val groupF = fromKvpCollection(kvp.kvpCollection)
        (path, _) => //Ignore fieldName here
          groupF(path).andThen(_.map(nv => nv.map(_.asInstanceOf[A])))
    }

  override def kvpCoproduct[C <: Coproduct](
    value: KvpCoproduct[String, ALG, C]
  ): Path[String] => ResultSet => Either[ExtractionErrors[String], CanBeOmitted[String, C]] = {
    path =>
      { rs =>
        {
          val dtype = rs.getString("dtype")
          coproductWithDtype(value, rs, dtype, path)
        }
      }
  }

  private def coproductWithDtype[C <: Coproduct](
    kvp: KvpCoproduct[String, ALG, C],
    rs: ResultSet,
    dtype: String,
    path: Path[String]
  ): Either[ExtractionErrors[String], CanBeOmitted[String, C]] = {
    kvp match {
      case _: KvpCoNil[String, ALG] =>
        Right(Left(List(OmittedValue(kvp.typeNameOfA, kvp.typeNameOfA, path))))
      case head: KvpCoproductCollectionHead[String, ALG, a, c, C] => {
        if (head.typeNameOfA.capitalize == dtype) {
          fromKvpCollection(head.kvpCollection)(path)(rs).map(nv => nv.map(Inl(_).asInstanceOf[C]))
        } else {
          coproductWithDtype(head.kvpTail, rs, dtype, path).map(nv =>
            nv.map(Inr(_).asInstanceOf[C])
          )
        }
      }
    }
  }

}
