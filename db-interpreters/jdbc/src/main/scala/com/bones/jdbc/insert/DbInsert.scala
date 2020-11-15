package com.bones.jdbc.insert

import java.sql._

import cats.data.NonEmptyList
import com.bones.data.Error.{ExtractionError, ExtractionErrors, SystemError}
import com.bones.data.KeyDefinition.CoproductDataDefinition
import com.bones.data.KvpCollection.headTypeName
import com.bones.data._
import com.bones.data.template.KvpCollectionFunctor
import com.bones.jdbc.DbUtil._
import com.bones.jdbc.column.ColumnNameInterpreter
import com.bones.jdbc.rs.ResultSetInterpreter
import javax.sql.DataSource
import shapeless.{::, Coproduct, HList, HNil, Inl, Inr, Nat}

import scala.util.control.NonFatal

trait DbInsert[ALG[_]]
    extends KvpCollectionFunctor[
      String,
      ALG,
      Lambda[A => (Index, A) => (Index, List[(ColumnName, SetValue)])]] {

  def resultSetInterpreter: ResultSetInterpreter[ALG]
  def customInterpreter: DbInsertValue[ALG]
  def columnNameInterpreter: ColumnNameInterpreter[ALG]

  def insertQuery[A, ID](
    collection: KvpCollection[String, ALG, A],
    idSchema: KvpCollection[String, ALG, ID])
    : DataSource => A => Either[ExtractionErrors[String], ID] = {
    val iq = insertQueryWithConnection(collection, idSchema)
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

  def batchInsertQueryWithConnection[A](
    collection: KvpCollection[String, ALG, A]
  ): Seq[A] => Connection => Either[ExtractionErrors[String], Int] = {
    val tableName = camelToSnake(headTypeName(collection).getOrElse("Unknown"))
    val updates = fromKvpCollection(collection)

    (l: Seq[A]) =>
      {
        val af = l.map(a => updates(1, a))
        val groups = af.groupBy(res => (res._1, res._2.map(_._1)))

        con: Connection =>
          {
            val originalAc = con.getAutoCommit
            con.setAutoCommit(false)
            try {
              val updates = groups.map(item => {
                val sql = sqlString(tableName, item._1._2)
                val statement = con.prepareStatement(sql, Statement.RETURN_GENERATED_KEYS)
                val setValueBatches = item._2.map(_._2.map(_._2))
                setValueBatches.foreach(setValues => {
                  setValues.foreach(f => f(statement))
                  statement.addBatch()
                })
                statement.executeBatch()
              })
              con.commit()
              Right(updates.map(_.sum).sum)
            } catch {
              case NonFatal(ex) => Left(NonEmptyList.one(SystemError(List.empty, ex, None)))
            } finally {
              con.setAutoCommit(originalAc)
            }
          }

      }

  }

  private def sqlString(tableName: String, columns: List[ColumnName]) =
    s"""insert into $tableName ( ${columns
      .mkString(",")} ) values ( ${columns.map(_ => "?").mkString(",")}  )"""

  def insertQueryWithConnection[A, ID](
    collection: KvpCollection[String, ALG, A],
    idSchema: KvpCollection[String, ALG, ID])
    : A => Connection => Either[ExtractionErrors[String], ID] = {
    val tableName = camelToSnake(headTypeName(collection).getOrElse("Unknown"))
    val updates = fromKvpCollection(collection)
    val rs = resultSetInterpreter.generateResultSet(idSchema)
    a: A =>
      {
        val result = updates(1, a)
        val sql = sqlString(tableName, result._2.map(_._1))
        con: Connection =>
          {
            val statement = con.prepareStatement(sql, Statement.RETURN_GENERATED_KEYS)
            try {
              result._2.map(_._2).foreach(f => f(statement))
              statement.executeUpdate()
              val generatedKeys = statement.getGeneratedKeys

              try {
                if (generatedKeys.next)
                  rs.apply(List.empty).apply(generatedKeys)
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

  override def kvpNil(
    kvp: KvpNil[String, ALG]): (Index, HNil) => (Index, List[(ColumnName, SetValue)]) =
    (i, h) => (i, List.empty)

  override def kvpWrappedHList[A, H <: HList, HL <: Nat](
    wrappedHList: KvpWrappedHList[String, ALG, A, H, HL])
    : (Index, A) => (Index, List[(ColumnName, SetValue)]) = {
    val wrappedF = fromKvpCollection(wrappedHList.wrappedEncoding)
    (i, a) =>
      wrappedF(i, wrappedHList.fAtoH(a))
  }

  override def kvpWrappedCoproduct[A, C <: Coproduct](
    wrappedCoproduct: KvpWrappedCoproduct[String, ALG, A, C])
    : (Index, A) => (Index, List[(ColumnName, SetValue)]) = {
    val wrappedF = fromKvpCollection(wrappedCoproduct.wrappedEncoding)
    (i, a) =>
      {
        wrappedF(i, wrappedCoproduct.fAtoC(a))
      }
  }

  override def kvpSingleValueHead[H, T <: HList, TL <: Nat, O <: H :: T](
    kvp: KvpSingleValueHead[String, ALG, H, T, TL, O])
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
    TL <: Nat](kvp: KvpHListCollectionHead[String, ALG, HO, NO, H, HL, T, TL])
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
    value: KvpCoproduct[String, ALG, C]): (Index, C) => (Index, List[(ColumnName, SetValue)]) = {

    value match {
      case _: KvpCoNil[String, ALG] @unchecked =>
        (i, _) =>
          (i, List.empty) //Should not actually get here
      case kvp: KvpCoproductCollectionHead[String, ALG, a, c, C] => {
        val headF = fromKvpCollection(kvp.kvpCollection)
        val tailF = kvpCoproduct(kvp.kvpTail)
        (i, c) =>
          {
            c match {
              case Inl(head) => {
                val dtype: (String, SetValue) =
                  ("dtype", ps => ps.setString(i, kvp.typeNameOfA.capitalize))
                val result = headF(i + 1, head.asInstanceOf[a])
                (result._1, dtype :: result._2)
              }
              case Inr(tail) => tailF(i, tail.asInstanceOf[c])
            }
          }
      }
    }

  }

  def determineValueDefinition[A](
    coproduct: CoproductDataDefinition[String, ALG, A]): InsertPair[A] = {
    coproduct match {
      case Left(kvp)  => valueDefinition(kvp)
      case Right(alg) => customInterpreter.insertPair(alg)
    }
  }

  def valueDefinition[A](fgo: HigherOrderValue[String, ALG, A]): InsertPair[A] =
    fgo match {
      case op: OptionalValue[String, ALG, b] @unchecked =>
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
      case ld: ListData[String, ALG, t] @unchecked => ???
      case ed: EitherData[String, ALG, a, b] @unchecked =>
        val leftF = determineValueDefinition(ed.definitionA)
        val rightF = determineValueDefinition(ed.definitionB)

        key =>
          val (leftRename, rightRename) = calculateRename(ed, key)

          { (index, input) =>
            input match {
              case Left(a) => {
                val result = leftF(key)(index, a)
                val renamed = rename(result._2, leftRename)
                (result._1, renamed)
              }
              case Right(b) => {
                val result = rightF(key)(index, b)
                val renamed = rename(result._2, rightRename)
                (result._1, renamed)
              }
            }
          }
      case kvp: KvpCollectionValue[String, ALG, a] @unchecked =>
        val groupF = fromKvpCollection(kvp.kvpCollection)
        k =>
          { (index, a) =>
            groupF(index, a)
          }
    }

  /**
    * If there are similar named columns from either side of either,
    * then we need to rename them using the typeName.  This method
    * finds these columns.
    * @param ed
    * @tparam aa
    * @tparam bb
    * @return Two maps, where _1 is the left columns to rename and _2 are the
    *         right columns to rename.
    */
  private def calculateRename[aa, bb](
    ed: EitherData[String, ALG, aa, bb],
    key: String): (Map[String, String], Map[String, String]) = {
    val leftNames = columnNameInterpreter.determineValueDefinition(ed.definitionA)(key)
    val rightNames = columnNameInterpreter.determineValueDefinition(ed.definitionB)(key)

    val colsToRename = leftNames.intersect(rightNames)
    val leftToRename = Map.from(
      colsToRename.map(col => (col, col + "_" + camelToSnake(ed.typeNameOfA)))
    )
    val rightToRename = Map.from(
      colsToRename.map(col => (col, col + "_" + camelToSnake(ed.typeNameOfB)))
    )
    (leftToRename, rightToRename)
  }

  /** Rename the ColumnName if it exists in the map. */
  private def rename(
    l: List[(ColumnName, SetValue)],
    map: Map[String, String]): List[(ColumnName, SetValue)] = {
    l.map(col => {
      map.get(col._1) match {
        case Some(newName) => (newName, col._2)
        case None          => col
      }
    })
  }

}
