package com.bones.jdbc.update

import java.sql.{Connection, PreparedStatement, SQLException}

import cats.data.NonEmptyList
import com.bones.data.Error.{ExtractionError, SystemError}
import com.bones.data._
import com.bones.data.values.CNilF
import com.bones.jdbc.DbUtil._
import com.bones.jdbc.IdDefinition
import com.bones.jdbc.column.DbColumnInterpreter
import com.bones.jdbc.column.DbColumnInterpreter.ColumnInterpreter
import com.bones.jdbc.insert.DbInsertValues
import javax.sql.DataSource
import shapeless.{:+:, ::, Coproduct, HList, HNil, Inl, Inr, Nat}

/** insert into table (field1, field2, field3) values (:value1, :value2, :value3) */
object DbUpdateValues {

  object CustomDbUpdateInterpreter {

    /** using kind projector allows us to create a new interpreter by merging two existing interpreters.
      * see https://stackoverflow.com/a/60561575/387094
      * */
    def merge[L[_], R[_] <: Coproduct, A, OUT](
      li: CustomDbUpdateInterpreter[L],
      ri: CustomDbUpdateInterpreter[R]
    ): CustomDbUpdateInterpreter[Lambda[A => L[A] :+: R[A]]] =
      new CustomDbUpdateInterpreter[Lambda[A => L[A] :+: R[A]]] {

        override def definitionResult[A](alg: L[A] :+: R[A]): (Index, Key) => DefinitionResult[A] =
          alg match {
            case Inl(l) => li.definitionResult(l)
            case Inr(r) => ri.definitionResult(r)
          }

      }

    implicit class InterpreterOps[ALG[_], OUT](val base: CustomDbUpdateInterpreter[ALG])
        extends AnyVal {
      def ++[R[_] <: Coproduct](
        r: CustomDbUpdateInterpreter[R]
      ): CustomDbUpdateInterpreter[Lambda[A => ALG[A] :+: R[A]]] =
        merge(base, r)

    }

    object CNilUpdateInterpreter extends CustomDbUpdateInterpreter[CNilF] {
      override def definitionResult[A](alg: CNilF[A]): (Index, Key) => DefinitionResult[A] =
        sys.error("Unreachable code")
    }
  }

  trait CustomDbUpdateInterpreter[ALG[_]] {
    def definitionResult[A](alg: ALG[A]): (Index, Key) => DefinitionResult[A]
  }

  type FieldName = String
  type FieldValue = String
  type Key = String
  type AssignmentString = String
  type SetValue = PreparedStatement => Unit
  type SetNull = PreparedStatement => Unit
  type Index = Int
  type ID = Long

  case class DefinitionResult[A](
    lastIndex: Index,
    assignmentStatements: List[(AssignmentString, SetNull)],
    predicates: A => List[SetValue])

  def updateQueryCustomAlgebra[ALG[_], A, ID](
                                               bonesSchema: KvpCollection[ALG, A],
                                               customDbUpdateInterpreter: CustomDbUpdateInterpreter[ALG],
                                               columnInterpreter: ColumnInterpreter[ALG],
                                               idDef: IdDefinition[ALG, ID])
    : DataSource => (ID, A) => Either[NonEmptyList[ExtractionError], (ID, A)] = {
    val uq = updateQueryWithConnectionCustomAlgebra(bonesSchema, customDbUpdateInterpreter, columnInterpreter, idDef)
    ds => (id, a) =>
      withDataSource[(ID, A)](ds)(con => uq(id, a)(con))
  }

  def updateQueryWithConnectionCustomAlgebra[ALG[_], A, ID](
                                                             bonesSchema: KvpCollection[ALG, A],
                                                             customDbUpdateInterpreter: CustomDbUpdateInterpreter[ALG],
                                                             columnInterpreter: ColumnInterpreter[ALG],
                                                             idDef: IdDefinition[ALG, ID]): (ID, A) => Connection => Either[NonEmptyList[SystemError], (ID, A)] =
    bonesSchema match {
      case x: HListConvert[ALG, h, n, b] @unchecked => {
        val tableName = camelToSnake(x.manifestOfA.runtimeClass.getSimpleName)
        val updates = kvpHList(x.from, customDbUpdateInterpreter)(1)
        val idIndex = updates.lastIndex
        val idUpdateFunction =
          DbUpdateValues.valueDefinition(idDef.asSchema, customDbUpdateInterpreter)(idIndex,idDef.key)
        // TODO this does not handle null/none case
        val sql =
          s"""update ${tableName} set ${updates.assignmentStatements
            .map(_._1)
            .mkString(",")} where ${idUpdateFunction.assignmentStatements.map(_._1).mkString(" and ")}"""
        (id: ID, a: A) =>
          {
            (con: Connection) =>
              {
                val statement = con.prepareCall(sql)
                try {
                  updates
                    .predicates(x.fAtoH(a))
                    .foreach(f => f(statement))
                  idUpdateFunction.predicates.apply(id).foreach(f => f.apply(statement))
                  statement.execute()
                  Right((id, a))
                } catch {
                  case e: SQLException =>
                    Left(NonEmptyList.one(SystemError(e, Some(sql))))
                } finally {
                  statement.close()
                }
              }
          }
      }
    }

  def kvpHList[ALG[_], H <: HList, HL <: Nat](
    group: KvpHList[ALG, H, HL],
    customDbUpdateInterpreter: CustomDbUpdateInterpreter[ALG]): Index => DefinitionResult[H] = {
    group match {
      case kvp: KvpNil[_] =>
        i =>
          DefinitionResult(i, List.empty, (h: HNil) => List.empty)
      case op: KvpSingleValueHead[ALG, h, t, tl, a] => {
        val headF =
          determineValueDefinition(op.fieldDefinition.dataDefinition, customDbUpdateInterpreter)
        val tailF = kvpHList(op.tail, customDbUpdateInterpreter)
        (i: Index) =>
          {
            val headResult = headF(i, op.fieldDefinition.key)
            val tailResult = tailF(headResult.lastIndex)
            implicit val hCons = op.isHCons
            val f: H => List[SetValue] = (h: H) => {
              val hAsA: a = h.asInstanceOf[a]
              val headSetValues = headResult.predicates(hAsA.head)
              val tailSetValue = tailResult.predicates(hAsA.tail)
              headSetValues ::: tailSetValue
            }
            DefinitionResult(
              tailResult.lastIndex,
              headResult.assignmentStatements ::: tailResult.assignmentStatements,
              f)
          }
      }
      case op: KvpConcreteTypeHead[ALG, a, ht, nt] @unchecked => {

        def fromBones[A](bonesSchema: KvpCollection[ALG, A]): Index => DefinitionResult[A] = {
          bonesSchema match {
            case hList: HListConvert[ALG, h, n, a] =>
              index =>
                {
                  val dr: DefinitionResult[h] =
                    kvpHList(hList.from, customDbUpdateInterpreter)(index)
                  val as: A => List[SetValue] = a => dr.predicates(hList.fAtoH(a))
                  DefinitionResult(dr.lastIndex, dr.assignmentStatements, as)
                }
            case co: KvpCoproductConvert[ALG, c, a] =>
              index =>
                ???
          }
        }

        val headF = fromBones(op.collection)
        val tailF = kvpHList(op.tail, customDbUpdateInterpreter)
        (i: Index) =>
          {
            val headResult = headF(i)
            val tailResult = tailF(headResult.lastIndex)
            implicit val isHCons = op.isHCons
            val f = (input: a :: ht) => {
              val headList = headResult.predicates(input.head)
              val tailList = tailResult.predicates(input.tail)
              (headList ::: tailList)
            }
            DefinitionResult[H](
              tailResult.lastIndex,
              headResult.assignmentStatements ::: tailResult.assignmentStatements,
              f)
          }
      }
      case op: KvpHListHead[ALG, a, al, h, hl, t, tl] @unchecked => {
        val headF = kvpHList(op.head, customDbUpdateInterpreter)
        val tailF = kvpHList(op.tail, customDbUpdateInterpreter)
        (i: Index) =>
          {
            val headResult = headF(i)
            val tailResult = tailF(headResult.lastIndex)
            val f = (input: H) => {
              val hSplit = op.split(input)
              val headList = headResult.predicates(hSplit._1)
              val tailList = tailResult.predicates(hSplit._2)
              (headList ::: tailList)
            }
            DefinitionResult[H](
              tailResult.lastIndex,
              headResult.assignmentStatements ::: tailResult.assignmentStatements,
              f)
          }
      }
    }
  }

  /** Create the return type for valueDefinition given the arguments */
  def psF[A](
    f: Index => (PreparedStatement, A) => Unit,
    sqlType: Int): (Index, Key) => DefinitionResult[A] =
    (index, key) => {
      val updateString = s"${camelToSnake(key)} = ?"
      val fI = f(index)
      val psNull: SetNull = ps => ps.setNull(index, sqlType)
      val setValue: A => List[SetValue] = a => {
        val setValueF: PreparedStatement => Unit = ps => fI(ps, a)
        List(setValueF)
      }
      DefinitionResult(index + 1, List((updateString, psNull)), setValue)
    }

  def determineValueDefinition[ALG[_], A](
    valueDef: Either[KvpCollection[ALG,A], ALG[A]],
    customDbUpdateInterpreter: CustomDbUpdateInterpreter[ALG]
  ): (Index, Key) => DefinitionResult[A] =
    valueDef match {
      case Left(kvp)  => valueDefinition[ALG, A](kvp, customDbUpdateInterpreter)
      case Right(alg) => customDbUpdateInterpreter.definitionResult(alg)
    }

  def valueDefinition[ALG[_], A](
    fgo: KvpCollection[ALG,A],
    customDbUpdateInterpreter: CustomDbUpdateInterpreter[ALG])
    : (Index, Key) => DefinitionResult[A] =
    fgo match {
      case op: OptionalKvpValueDefinition[ALG, b] @unchecked =>
        val valueF = determineValueDefinition(op.valueDefinitionOp, customDbUpdateInterpreter)
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
      case ld: ListData[ALG, t] @unchecked      => ???
      case ed: EitherData[ALG, a, b] @unchecked => ???
      case kvp: KvpHListValue[ALG, h, hl] @unchecked => {
        val groupF = kvpHList(kvp.kvpHList, customDbUpdateInterpreter)
        (i, k) =>
          {
            val result = groupF(i)
            val fa = (a: A) => result.predicates(a.asInstanceOf[h])
            DefinitionResult(result.lastIndex, result.assignmentStatements, fa)
          }
      }
      case x: HListConvert[ALG, a, al, b] @unchecked =>
        val groupF = kvpHList(x.from, customDbUpdateInterpreter)
        (i, k) =>
          {
            val result = groupF(i)
            val fa = (input: A) => result.predicates(x.fAtoH(input))
            DefinitionResult(result.lastIndex, result.assignmentStatements, fa)
          }
    }

}
