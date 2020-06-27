package com.bones.jdbc.rs

import java.sql.{ResultSet, SQLException}

import cats.data.NonEmptyList
import com.bones.KvpValue
import com.bones.Util
import com.bones.data.Error.{ExtractionError, RequiredValue, SystemError}
import com.bones.data.KeyValueDefinition.CoproductDataDefinition
import com.bones.data._
import com.bones.jdbc.DbUtil.camelToSnake
import com.bones.jdbc.FindInterpreter.{FieldName, Path}
import shapeless.{HList, HNil, Nat}

/** Responsible for converting a result set into the result type */
object ResultSetInterpreter {

  protected def kvpHList[ALG[_], H <: HList, N <: Nat](
    group: KvpHList[ALG, H, N],
    customInterpreter: ResultSetValue[ALG])
    : Path => ResultSet => Either[NonEmptyList[ExtractionError], H] =
    group match {
      case nil: KvpNil[_] =>
        path => rs =>
          Right(HNil)
      case op: KvpSingleValueHead[ALG, h, t, tl, a] @unchecked =>
        path =>
          {
            val newPath = op.fieldDefinition.key :: path
            val rsToHead = determineValueDefinition(
              op.fieldDefinition.dataDefinition,
              customInterpreter)(newPath, camelToSnake(op.fieldDefinition.key))
            val rsToTail = kvpHList(op.tail, customInterpreter)(path)
            rs =>
              {
                Util.eitherMap2(rsToHead(rs), rsToTail(rs))((l1: h, l2: t) => {
                  op.isHCons.cons(l1, l2)
                })
              }
          }
      case op: KvpConcreteTypeHead[ALG, a, ht, nt] @unchecked =>
        def fromSchema[A](bonesSchema: KvpCollection[ALG, A])
          : Path => ResultSet => Either[NonEmptyList[ExtractionError], A] =
          bonesSchema match {
            case hList: HListConvert[ALG, hh, nn, A] @unchecked => { path => resultSet =>
              {
                val resultHH = kvpHList(hList.from, customInterpreter)(path)(resultSet)
                resultHH.map(hh => hList.fHtoA(hh))
              }
            }
            case co: KvpCoproductConvert[ALG, c, a] => ???
          }

        val headF = fromSchema(op.collection)
        val tailF = kvpHList(op.tail, customInterpreter)
        path =>
          {
            val rsToHead = headF(path)
            val rsToTail = tailF(path)
            rs =>
              {
                Util.eitherMap2(rsToHead(rs), rsToTail(rs))((l1: a, l2: ht) => {
                  op.isHCons.cons(l1, l2)
                })
              }
          }
      case op: KvpHListHead[ALG, a, al, h, hl, t, tl] @unchecked =>
        val headF = kvpHList(op.head, customInterpreter)
        val tailF = kvpHList(op.tail, customInterpreter)
        path =>
          {
            val rsToHead = headF(path)
            val rsToTail = tailF(path)
            rs =>
              {
                Util.eitherMap2(rsToHead(rs), rsToTail(rs))((l1: h, l2: t) => {
                  op.prepend(l1, l2)
                })
              }
          }
    }

  def determineValueDefinition[ALG[_], A](
    coproduct: CoproductDataDefinition[ALG, A],
    customInterpreter: ResultSetValue[ALG])
    : (Path, FieldName) => ResultSet => Either[NonEmptyList[ExtractionError], A] =
    coproduct match {
      case Left(kvp)  => valueDefinition(kvp, customInterpreter)
      case Right(alg) => customInterpreter.resultSet(alg)
    }

  def generateResultSet[ALG[_], A](collection: KvpCollection[ALG, A], values: ResultSetValue[ALG]): Path => ResultSet => Either[NonEmptyList[ExtractionError], A] =
    path => valueDefinition(collection, values)(path, "")

  def valueDefinition[ALG[_], A](
    fgo: KvpCollection[ALG,A],
    customInterpreter: ResultSetValue[ALG])
    : (Path, FieldName) => ResultSet => Either[NonEmptyList[ExtractionError], A] =
    fgo match {
      case op: OptionalKvpValueDefinition[ALG, a] @unchecked =>
        (path, fieldName) =>
          val child =
            determineValueDefinition(op.valueDefinitionOp, customInterpreter)(path, fieldName)
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
            val leftField = determineValueDefinition(ed.definitionA, customInterpreter)(
              path,
              "left_" + fieldName)(rs)
            val result = leftField match {
              //if the error is that the left is required, we will check the right.
              case Left(nel) =>
                if (nel.length == 1) {
                  nel.head match {
                    case RequiredValue(path, op) if op == ed.definitionA => {
                      determineValueDefinition(ed.definitionB, customInterpreter)(
                        path,
                        "right_" + fieldName)(rs).map(Right(_))
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

      case kvp: KvpHListValue[ALG, h, hl] @unchecked =>
        val groupF = kvpHList(kvp.kvpHList, customInterpreter)
        (path, _) => //Ignore fieldName here
          groupF(path).andThen(_.map(_.asInstanceOf[A]))

      case x: HListConvert[ALG, a, al, b] @unchecked =>
        val groupF = kvpHList(x.from, customInterpreter)
        (path, _) =>
          groupF(path).andThen(_.map(x.fHtoA))
    }

  def catchSql[A](f: => A, path: Path, op: KvpValue[_]): Either[NonEmptyList[ExtractionError], A] =
    try {
      val result = f
      if (result == null) {
        Left(NonEmptyList.one(RequiredValue(path, Right(op))))
      } else {
        Right(result)
      }
    } catch {
      case ex: SQLException =>
        Left(NonEmptyList.one(SystemError(path, ex, None)))
    }

}
