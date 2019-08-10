package com.bones.jdbc

import java.sql.{Connection, PreparedStatement, SQLException, Types}
import java.time.{LocalDate, LocalDateTime, ZoneOffset}

import cats.data.NonEmptyList
import com.bones.crud.WithId
import com.bones.data.Error.{ExtractionError, SystemError}
import com.bones.data.Value._
import com.bones.jdbc.DbUtil._
import javax.sql.DataSource
import shapeless.{HList, HNil, Nat}

/** insert into table (field1, field2, field3) values (:value1, :value2, :value3) */
object DbUpdateValues {

  type FieldName = String
  type FieldValue = String
  type Key = String
  type UpdateString = String
  type SetValue = PreparedStatement => Unit
  type SetNull = PreparedStatement => Unit
  type Index = Int
  type ID = Long

  case class DefinitionResult[A](
      lastIndex: Index,
      predefineUpdateStatements: List[(UpdateString, SetNull)],
      actionableUpdateStatements: A => List[SetValue])

  def updateQuery[A](bonesSchema: BonesSchema[A]): DataSource => (
      Long,
      A) => Either[NonEmptyList[ExtractionError], WithId[Long, A]] = {
    val uq = updateQueryWithConnection(bonesSchema)
    ds => (id, a) =>
      withDataSource[WithId[Long, A]](ds)(con => uq(id, a)(con))
  }

  def updateQueryWithConnection[A](bonesSchema: BonesSchema[A])
    : (Long,
       A) => Connection => Either[NonEmptyList[SystemError], WithId[Long, A]] =
    bonesSchema match {
      case x: HListConvert[h, n, b] => {
        val tableName = camelToSnake(x.manifestOfA.runtimeClass.getSimpleName)
        val updates = valueDefinition(x)(1, tableName)
        (id: Long, a: A) =>
          {
            val sql =
              s"""update ${tableName} set ${updates.predefineUpdateStatements
                .map(_._1)
                .mkString(",")} where id = ?"""
            (con: Connection) =>
              {
                val statement = con.prepareCall(sql)
                try {
                  updates
                    .actionableUpdateStatements(a)
                    .foreach(f => f(statement))
                  statement.setLong(updates.lastIndex, id)
                  statement.execute()
                  Right(WithId(id, a))
                } catch {
                  case e: SQLException =>
                    Left(
                      NonEmptyList.one(SystemError(List.empty, e, Some(sql))))
                } finally {
                  statement.close()
                }
              }
          }
      }
    }

  def kvpHList[H <: HList, HL <: Nat](
      group: KvpHList[H, HL]): Index => DefinitionResult[H] = {
    group match {
      case KvpNil =>
        i =>
          DefinitionResult(i, List.empty, (h: HNil) => List.empty)
      case op: KvpSingleValueHead[h, t, tl, a] => {
        val headF = valueDefinition(op.fieldDefinition.op)
        val tailF = kvpHList(op.tail)
        (i: Index) =>
          {
            val headResult = headF(i, op.fieldDefinition.key)
            val tailResult = tailF(headResult.lastIndex)
            val f: H => List[SetValue] = (h: H) => {
              val headSetValues = headResult.actionableUpdateStatements(h.head)
              val tailSetValue = tailResult.actionableUpdateStatements(h.tail)
              headSetValues ::: tailSetValue
            }
            DefinitionResult(
              tailResult.lastIndex,
              headResult.predefineUpdateStatements ::: tailResult.predefineUpdateStatements,
              f)
          }
      }
      case op: KvpConcreteTypeHead[a, ht, nt, ho, xl, xll] => {
        val headF = kvpHList(op.hListConvert.from)
        val tailF = kvpHList(op.tail)
        (i: Index) =>
          {
            val headResult = headF(i)
            val tailResult = tailF(headResult.lastIndex)
            val f = (input: H) => {
              val headList = headResult.actionableUpdateStatements(
                op.hListConvert.fAtoH(input.head))
              val tailList = tailResult.actionableUpdateStatements(input.tail)
              (headList ::: tailList)
            }
            DefinitionResult[H](
              tailResult.lastIndex,
              headResult.predefineUpdateStatements ::: tailResult.predefineUpdateStatements,
              f)
          }
      }
      case op: KvpHListHead[a, al, h, hl, t, tl] => {
        val headF = kvpHList(op.head)
        val tailF = kvpHList(op.tail)
        (i: Index) =>
          {
            val headResult = headF(i)
            val tailResult = tailF(headResult.lastIndex)
            val f = (input: H) => {
              val hSplit = op.split(input)
              val headList = headResult.actionableUpdateStatements(hSplit._1)
              val tailList = tailResult.actionableUpdateStatements(hSplit._2)
              (headList ::: tailList)
            }
            DefinitionResult[H](
              tailResult.lastIndex,
              headResult.predefineUpdateStatements ::: tailResult.predefineUpdateStatements,
              f)
          }
      }
    }
  }

  /** Create the return type for valueDefinition given the arguments */
  private def psF[A](f: Index => (PreparedStatement, A) => Unit,
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

  def valueDefinition[A](
      fgo: KvpValue[A]): (Index, Key) => DefinitionResult[A] =
    fgo match {
      case op: OptionalKvpValueDefinition[b] =>
        val valueF = valueDefinition(op.valueDefinitionOp)
        (i, k) =>
          val ops = valueF(i, k)

          val f: A => List[SetValue] = (a: A) => {
            a match {
              case Some(b) =>
                ops.actionableUpdateStatements(b)
              case None => {
                // Instead of calling the sub statements, we set them all to null
                ops.predefineUpdateStatements.map(_._2)
              }
            }
          }
          ops.copy(actionableUpdateStatements = f)
      case ob: BooleanData =>
        psF(i => (ps, a) => ps.setBoolean(i, a), Types.BOOLEAN)
      case rs: StringData =>
        psF(i => (ps, a) => ps.setString(i, a), Types.LONGVARCHAR)
      case ri: IntData => psF(i => (ps, a) => ps.setInt(i, a), Types.INTEGER)
      case ri: LongData => psF(i => (ps, a) => ps.setLong(i, a), Types.BIGINT)
      case uu: UuidData =>
        psF(i => (ps, a) => ps.setString(i, a.toString), Types.VARCHAR)
      case dd: DateTimeData =>
        psF((i: Index) =>
              (ps: PreparedStatement, a: LocalDateTime) =>
                ps.setDate(i, new java.sql.Date(a.toInstant(ZoneOffset.UTC).toEpochMilli)),
            Types.DATE)
      case ld: LocalDateData =>
        psF((i: Index) =>
          (ps:PreparedStatement, a: LocalDate) =>
            ps.setDate(i, new java.sql.Date(a.atStartOfDay.toInstant(ZoneOffset.UTC).toEpochMilli)),
          Types.DATE)
      case fd: FloatData => psF(i => (ps, a) => ps.setFloat(i, a), Types.FLOAT)
      case fd: DoubleData => psF(i => (ps, a) => ps.setDouble(i, a), Types.DOUBLE)
      case bd: BigDecimalData =>
        psF[BigDecimal](i => (ps, a) => ps.setBigDecimal(i, a.underlying),
                        Types.NUMERIC)
      case ba: ByteArrayData =>
        psF[Array[Byte]](i => (ps, a) => ps.setBytes(i, a), Types.BINARY)
      case ld: ListData[t]      => ???
      case ed: EitherData[a, b] => ???
      case esd: EnumerationStringData[a] =>
        psF(i => (ps, a) => ps.setString(i, a.toString), Types.VARCHAR)
      case kvp: KvpHListValue[h, hl] => {
        val groupF = kvpHList(kvp.kvpHList)
        (i, k) =>
          {
            val result = groupF(i)
            val fa = (a: A) =>
              result.actionableUpdateStatements(a.asInstanceOf[h])
            DefinitionResult(result.lastIndex,
                             result.predefineUpdateStatements,
                             fa)
          }
      }
      case x: HListConvert[a, al, b] =>
        val groupF = kvpHList(x.from)
        (i, k) =>
          {
            val result = groupF(i)
            val fa = (input: A) =>
              result.actionableUpdateStatements(x.fAtoH(input))
            DefinitionResult(result.lastIndex,
                             result.predefineUpdateStatements,
                             fa)
          }
      case s: SumTypeData[a, b] =>
        val valueF = valueDefinition(s.from)
        (i, k) =>
          {
            val result = valueF(i, k)
            val fa = (input: A) =>
              result.actionableUpdateStatements(s.fba(input))
            DefinitionResult(result.lastIndex,
                             result.predefineUpdateStatements,
                             fa)
          }
    }

}
