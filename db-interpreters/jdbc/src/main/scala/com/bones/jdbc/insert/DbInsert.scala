package com.bones.jdbc.insert

import java.sql._

import cats.data.NonEmptyList
import com.bones.data.Error.{ExtractionError, SystemError}
import com.bones.data.KeyValueDefinition.CoproductDataDefinition
import com.bones.data._
import com.bones.jdbc.DbUtil._
import com.bones.jdbc.rs.{ResultSetInterpreter, ResultSetValue}
import javax.sql.DataSource
import shapeless.{::, HList, Nat}

object DbInsert {

  type FieldName = String
  type FieldValue = String
  type Key = String
  type ColumnName = String
  type SetValue = PreparedStatement => Unit
  type SetNull = PreparedStatement => Unit
  type Index = Int
  type InsertPair[A] = Key => (Index, A) => (Index, List[(ColumnName, SetValue)])

  def insertQuery[ALG[_], A, ID](
    collection: ConcreteValue[ALG, A],
    idSchema: ConcreteValue[ALG, ID],
    customInterpreter: DbInsertValue[ALG],
    resultSetValueInterpreter: ResultSetValue[ALG])
    : DataSource => A => Either[NonEmptyList[ExtractionError], (ID, A)] = {
    val iq = insertQueryWithConnectionCustomAlgebra(
      collection,
      idSchema,
      customInterpreter,
      resultSetValueInterpreter)
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

  def insertQueryWithConnectionCustomAlgebra[ALG[_], A, ID](
    collection: ConcreteValue[ALG, A],
    idSchema: ConcreteValue[ALG, ID],
    customInterpreter: DbInsertValue[ALG],
    resultSetInterpreter: ResultSetValue[ALG])
    : A => Connection => Either[NonEmptyList[ExtractionError], (ID, A)] = {
    val tableName = camelToSnake(collection.manifestOfA.runtimeClass.getSimpleName)
    val updates = valueDefinition(collection, customInterpreter)
    val rs = ResultSetInterpreter.determineValueDefinition(Left(idSchema), resultSetInterpreter)
    a: A =>
      {
        val result = updates("")(1, a)
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
                  rs.apply(List.empty, "id").apply(generatedKeys).map((_, a))
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

  def kvpHList[ALG[_], H <: HList, HL <: Nat](
    group: KvpCollection[ALG, H, HL],
    customInterpreter: DbInsertValue[ALG]): (Index, H) => (Index, List[(ColumnName, SetValue)]) = {
    group match {
      case nil: KvpNil[_] =>
        (i, h) =>
          (i, List.empty)
      case op: KvpSingleValueHead[ALG, h, t, tl, H] @unchecked => {
        val headF =
          determineValueDefinition(op.fieldDefinition.dataDefinition, customInterpreter)(
            op.fieldDefinition.key)
        val tailF = kvpHList(op.tail, customInterpreter)
        implicit val hCons = op.isHCons
        (i: Index, h: H) =>
          {
            val headResult = headF(i, h.head)
            val tailResult = tailF(headResult._1, h.tail)
            (tailResult._1, headResult._2 ::: tailResult._2)
          }
      }
      case op: KvpCollectionHead[ALG, a, al, h, hl, t, tl] @unchecked => {
        val headF = kvpHList(op.head, customInterpreter)
        val tailF = kvpHList(op.tail, customInterpreter)
        (i: Index, h: H) =>
          {
            val hSplit = op.split(h)
            val headList = headF(i, hSplit._1)
            val tailList = tailF(headList._1, hSplit._2)
            (tailList._1, headList._2 ::: tailList._2)
          }
      }
      case op: KvpConcreteValueHead[ALG, a, ht, nt] => {

        val headF = fromCollection(op.collection, customInterpreter)
        val tailF = kvpHList(op.tail, customInterpreter)

        (i: Index, h: a :: ht) =>
          {
            val headList = headF(i, h.head)
            val tailList = tailF(headList._1, h.tail)
            (tailList._1, headList._2 ::: tailList._2)
          }
      }
    }
  }

  private def fromCollection[ALG[_], A](
    bonesSchema: ConcreteValue[ALG, A],
    customInterpreter: DbInsertValue[ALG]): (Index, A) => (Index, List[(ColumnName, SetValue)]) = {
    bonesSchema match {
      case hListConvert: SwitchEncoding[ALG, h, n, a] =>
        val f = kvpHList(hListConvert.from, customInterpreter)
        (index, a) =>
          {
            val h = hListConvert.fAtoH(a)
            f(index, h)
          }
      case _ => ??? // TODO
    }
  }

  /** Create the return type for valueDefinition given the arguments */
  def psF[A](f: (PreparedStatement, Index, A) => Unit): InsertPair[A] =
    key => {
      val columnName = camelToSnake(key)
      (index: Index, a: A) =>
        {
          val setValue: SetValue = ps => {
            f(ps, index, a)
          }
          (index + 1, List((columnName, setValue)))
        }
    }

  def determineValueDefinition[ALG[_], A](
    coproduct: CoproductDataDefinition[ALG, A],
    customInterpreter: DbInsertValue[ALG]): InsertPair[A] = {
    coproduct match {
      case Left(kvp)  => valueDefinition(kvp, customInterpreter)
      case Right(alg) => customInterpreter.insertPair(alg)
    }
  }

  def valueDefinition[ALG[_], A](
    fgo: ConcreteValue[ALG, A],
    customInterpreter: DbInsertValue[ALG]): InsertPair[A] =
    fgo match {
      case op: OptionalValue[ALG, b] @unchecked =>
        val valueF = determineValueDefinition(op.valueDefinitionOp, customInterpreter)
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
      case kvp: KvpHListValue[ALG, h, hl] @unchecked =>
        val groupF = kvpHList(kvp.kvpHList, customInterpreter)
        k =>
          { (index, a) =>
            {
              groupF(index, a.asInstanceOf[h])
            }
          }
      case x: SwitchEncoding[ALG, a, al, b] @unchecked =>
        val groupF = kvpHList(x.from, customInterpreter)
        k =>
          { (index, h) =>
            {
              groupF(index, x.fAtoH(h))
            }
          }
      case co: CoproductSwitch[ALG, c, a]  => ??? // TODO
      case co: CoproductCollection[ALG, c] => ??? // TODO
    }

}
