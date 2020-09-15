package com.bones.jdbc.insert

import java.sql._

import cats.data.NonEmptyList
import com.bones.data.Error.{ExtractionError, SystemError}
import com.bones.data.KeyDefinition.CoproductDataDefinition
import com.bones.data.KvpCollection.headTypeName
import com.bones.data._
import com.bones.data.template.KvpCollectionFunctor
import com.bones.jdbc.DbUtil._
import com.bones.jdbc.rs.ResultSetInterpreter
import javax.sql.DataSource
import shapeless.{::, Coproduct, HList, HNil, Nat}

trait DbInsert[ALG[_]]
    extends KvpCollectionFunctor[
      ALG,
      Lambda[A => (Index, A) => (Index, List[(ColumnName, SetValue)])]] {

  def resultSetInterpreter: ResultSetInterpreter[ALG]
  def customInterpreter: DbInsertValue[ALG]

  def insertQuery[A, ID](collection: KvpCollection[ALG, A], idSchema: KvpCollection[ALG, ID])
    : DataSource => A => Either[NonEmptyList[ExtractionError], (ID, A)] = {
    val iq = insertQueryWithConnectionCustomAlgebra(collection, idSchema)
    ds =>
      { a =>
        {
          try {
            val con = ds.getConnection
            val result = iq(a)(con)
            con.close()
            result
          } catch {
            case ex: SQLException =>
              Left(
                NonEmptyList.one(SystemError(List.empty, ex, Some("Error retrieving connection"))))
          }
        }
      }
  }

  def insertQueryWithConnectionCustomAlgebra[A, ID](
    collection: KvpCollection[ALG, A],
    idSchema: KvpCollection[ALG, ID])
    : A => Connection => Either[NonEmptyList[ExtractionError], (ID, A)] = {
    val tableName = camelToSnake(headTypeName(collection).getOrElse("Unknown"))
    val updates = fromKvpCollection(collection)
    val rs = resultSetInterpreter.fromKvpCollection(idSchema)
    a: A =>
      {
        val result = updates(1, a)
        val sql = s"""insert into $tableName ( ${result._2
          .map(_._1)
          .mkString(",")} ) values ( ${result._2.map(_ => "?").mkString(",")}  )"""
        con: Connection =>
          {
            val statement = con.prepareStatement(sql, Statement.RETURN_GENERATED_KEYS)
            try {
              result._2.map(_._2).foreach(f => f(statement))
              statement.executeUpdate()
              val generatedKeys = statement.getGeneratedKeys

              try {
                if (generatedKeys.next)
                  rs.apply(List.empty).apply(generatedKeys).map((_, a))
                else throw new SQLException("Creating user failed, no ID obtained.")
              } finally {
                generatedKeys.close()
              }
            } catch {
              case e: SQLException =>
                Left(NonEmptyList.one(SystemError(e, Some("SQL Statement: " + sql))))
            } finally {
              statement.close()
            }
          }
      }
  }

  override def kvpNil(kvp: KvpNil[ALG]): (Index, HNil) => (Index, List[(ColumnName, SetValue)]) =
    (i, h) => (i, List.empty)

  override def kvpWrappedHList[A, H <: HList, HL <: Nat](
    wrappedHList: KvpWrappedHList[ALG, A, H, HL])
    : (Index, A) => (Index, List[(ColumnName, SetValue)]) = {
    val wrappedF = fromKvpCollection(wrappedHList.wrappedEncoding)
    (i, a) =>
      wrappedF(i, wrappedHList.fAtoH(a))
  }

  override def kvpWrappedCoproduct[A, C <: Coproduct](
    wrappedCoproduct: KvpWrappedCoproduct[ALG, A, C])
    : (Index, A) => (Index, List[(ColumnName, SetValue)]) = {
    val wrappedF = fromKvpCollection(wrappedCoproduct.wrappedEncoding)
    (i, a) =>
      wrappedF(i, wrappedCoproduct.fAtoC(a))
  }

  override def kvpSingleValueHead[H, T <: HList, TL <: Nat, O <: H :: T](
    kvp: KvpSingleValueHead[ALG, H, T, TL, O])
    : (Index, O) => (Index, List[(ColumnName, SetValue)]) = {
    val headF = kvp.head match {
      case Left(keyDef)         => determineValueDefinition(keyDef.dataDefinition)(keyDef.key)
      case Right(kvpCollection) => fromKvpCollection(kvpCollection)
    }
    val tailF = fromKvpCollection(kvp.tail)

    (i: Index, o: O) =>
      {
        val h = kvp.isHCons.head(o)
        val t = kvp.isHCons.tail(o)
        val headResult = headF(i, h)
        val tailResult = tailF(headResult._1, t)
        val nameValues = headResult._2 ::: tailResult._2
        (tailResult._1, nameValues)
      }
  }

  override def kvpHListCollectionHead[
    HO <: HList,
    NO <: Nat,
    H <: HList,
    HL <: Nat,
    T <: HList,
    TL <: Nat](kvp: KvpHListCollectionHead[ALG, HO, NO, H, HL, T, TL])
    : (Index, HO) => (Index, List[(ColumnName, SetValue)]) = {
    val headF = fromKvpCollection(kvp.head)
    val tailF = fromKvpCollection(kvp.tail)
    (i: Index, ho: HO) =>
      {
        val (h, t) = kvp.split(ho)
        val headResult = headF(i, h)
        val tailResult = tailF(headResult._1, t)
        val nameValues = headResult._2 ::: tailResult._2
        (tailResult._1, nameValues)
      }
  }

  override def kvpCoproduct[C <: Coproduct](
    value: KvpCoproduct[ALG, C]): (Index, C) => (Index, List[(ColumnName, SetValue)]) =
    throw new UnsupportedOperationException("coproduct currently not supported for DbInsert") // TODO: implement this

  def determineValueDefinition[A](coproduct: CoproductDataDefinition[ALG, A]): InsertPair[A] = {
    coproduct match {
      case Left(kvp)  => valueDefinition(kvp)
      case Right(alg) => customInterpreter.insertPair(alg)
    }
  }

  def valueDefinition[A](fgo: HigherOrderValue[ALG, A]): InsertPair[A] =
    fgo match {
      case op: OptionalValue[ALG, b] @unchecked =>
        val valueF = determineValueDefinition(op.valueDefinitionOp)
        key =>
          { (index: Index, a: A) =>
            {
              a match {
                case Some(b) => valueF(key)(index, b)
                case None    => (index, List.empty)
              }
            }
          }
      case ld: ListData[ALG, t] @unchecked      => ???
      case ed: EitherData[ALG, a, b] @unchecked => ???
      case kvp: KvpCollectionValue[ALG, a] @unchecked =>
        val groupF = fromKvpCollection(kvp.kvpCollection)
        k =>
          { (index, a) =>
            groupF(index, a)
          }
    }

}
