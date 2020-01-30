package com.bones.jdbc

import java.sql.{Connection, PreparedStatement, SQLException, Types}
import java.time.{LocalDate, LocalDateTime, ZoneOffset}

import cats.data.NonEmptyList
import com.bones.data.Error.{ExtractionError, SystemError}
import com.bones.data._
import com.bones.jdbc.DbUtil._
import com.bones.syntax.NoAlgebra
import javax.sql.DataSource
import shapeless.{HList, HNil, Nat, ::}

/** insert into table (field1, field2, field3) values (:value1, :value2, :value3) */
object DbUpdateValues {

  trait CustomDbUpdateInterpreter[ALG[_]] {
    def definitionResult[A](alg: ALG[A]): (Index, Key) => DefinitionResult[A]
  }

  object NoAlgebraInterpreter extends CustomDbUpdateInterpreter[NoAlgebra] {
    override def definitionResult[A](alg: NoAlgebra[A]): (Index, Key) => DefinitionResult[A] =
      sys.error("Unreachable code.")
  }

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

  def updateQuery[A](bonesSchema: BonesSchema[NoAlgebra, A])
    : DataSource => (Long, A) => Either[NonEmptyList[ExtractionError], (Long, A)] =
    updateQueryCustomAlgebra[NoAlgebra, A](bonesSchema, NoAlgebraInterpreter)

  def updateQueryCustomAlgebra[ALG[_], A](
    bonesSchema: BonesSchema[ALG, A],
    customDbUpdateInterpreter: CustomDbUpdateInterpreter[ALG])
    : DataSource => (Long, A) => Either[NonEmptyList[ExtractionError], (Long, A)] = {
    val uq = updateQueryWithConnectionCustomAlgebra(bonesSchema, customDbUpdateInterpreter)
    ds => (id, a) =>
      withDataSource[(Long, A)](ds)(con => uq(id, a)(con))
  }

  def updateQueryWithConnection[ALG[_], A](bonesSchema: BonesSchema[NoAlgebra, A])
    : (Long, A) => Connection => Either[NonEmptyList[SystemError], (Long, A)] =
    updateQueryWithConnectionCustomAlgebra[NoAlgebra, A](bonesSchema, NoAlgebraInterpreter)

  def updateQueryWithConnectionCustomAlgebra[ALG[_], A](
    bonesSchema: BonesSchema[ALG, A],
    customDbUpdateInterpreter: CustomDbUpdateInterpreter[ALG])
    : (Long, A) => Connection => Either[NonEmptyList[SystemError], (Long, A)] =
    bonesSchema match {
      case x: HListConvert[ALG, h, n, b] @unchecked => {
        val tableName = camelToSnake(x.manifestOfA.runtimeClass.getSimpleName)
        val updates = kvpHList(x.from, customDbUpdateInterpreter)(1)
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
                    .actionableUpdateStatements(x.fAtoH(a))
                    .foreach(f => f(statement))
                  statement.setLong(updates.lastIndex, id)
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
        val headF = determineValueDefinition(op.fieldDefinition.op, customDbUpdateInterpreter)
        val tailF = kvpHList(op.tail, customDbUpdateInterpreter)
        (i: Index) =>
          {
            val headResult = headF(i, op.fieldDefinition.key)
            val tailResult = tailF(headResult.lastIndex)
            implicit val hCons = op.isHCons
            val f: H => List[SetValue] = (h: H) => {
              val hAsA: a = h.asInstanceOf[a]
              val headSetValues = headResult.actionableUpdateStatements(hAsA.head)
              val tailSetValue = tailResult.actionableUpdateStatements(hAsA.tail)
              headSetValues ::: tailSetValue
            }
            DefinitionResult(
              tailResult.lastIndex,
              headResult.predefineUpdateStatements ::: tailResult.predefineUpdateStatements,
              f)
          }
      }
      case op: KvpConcreteTypeHead[ALG, a, ht, nt] @unchecked => {

        def fromBones[A](bonesSchema: BonesSchema[ALG, A]): Index => DefinitionResult[A] = {
          bonesSchema match {
            case hList: HListConvert[ALG, h, n, a] =>
              index =>
                {
                  val dr: DefinitionResult[h] =
                    kvpHList(hList.from, customDbUpdateInterpreter)(index)
                  val as: A => List[SetValue] = a => dr.actionableUpdateStatements(hList.fAtoH(a))
                  DefinitionResult(dr.lastIndex, dr.predefineUpdateStatements, as)
                }
            case co: KvpCoproductConvert[ALG, c, a] =>
              index =>
                ???
          }
        }

        val headF = fromBones(op.bonesSchema)
        val tailF = kvpHList(op.tail, customDbUpdateInterpreter)
        (i: Index) =>
          {
            val headResult = headF(i)
            val tailResult = tailF(headResult.lastIndex)
            implicit val isHCons = op.isHCons
            val f = (input: a :: ht) => {
              val headList = headResult.actionableUpdateStatements(input.head)
              val tailList = tailResult.actionableUpdateStatements(input.tail)
              (headList ::: tailList)
            }
            DefinitionResult[H](
              tailResult.lastIndex,
              headResult.predefineUpdateStatements ::: tailResult.predefineUpdateStatements,
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
  private def psF[A](
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
    valueDef: Either[KvpValue[A], ALG[A]],
    customDbUpdateInterpreter: CustomDbUpdateInterpreter[ALG]
  ): (Index, Key) => DefinitionResult[A] =
    valueDef match {
      case Left(kvp)  => valueDefinition[ALG, A](kvp, customDbUpdateInterpreter)
      case Right(alg) => customDbUpdateInterpreter.definitionResult(alg)
    }

  def valueDefinition[ALG[_], A](
    fgo: KvpValue[A],
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
      case ri: IntData   => psF(i => (ps, a) => ps.setInt(i, a), Types.INTEGER)
      case ri: ShortData => psF(i => (ps, a) => ps.setShort(i, a), Types.SMALLINT)
      case ri: LongData  => psF(i => (ps, a) => ps.setLong(i, a), Types.BIGINT)
      case uu: UuidData =>
        psF(i => (ps, a) => ps.setString(i, a.toString), Types.VARCHAR)
      case dd: LocalDateTimeData =>
        psF(
          (i: Index) =>
            (ps: PreparedStatement, a: LocalDateTime) =>
              ps.setDate(i, new java.sql.Date(a.toInstant(ZoneOffset.UTC).toEpochMilli)),
          Types.DATE)
      case ld: LocalDateData =>
        psF(
          (i: Index) =>
            (ps: PreparedStatement, a: LocalDate) =>
              ps.setDate(
                i,
                new java.sql.Date(a.atStartOfDay.toInstant(ZoneOffset.UTC).toEpochMilli)),
          Types.DATE)
      case fd: FloatData  => psF(i => (ps, a) => ps.setFloat(i, a), Types.FLOAT)
      case fd: DoubleData => psF(i => (ps, a) => ps.setDouble(i, a), Types.DOUBLE)
      case bd: BigDecimalData =>
        psF[BigDecimal](i => (ps, a) => ps.setBigDecimal(i, a.underlying), Types.NUMERIC)
      case ba: ByteArrayData =>
        psF[Array[Byte]](i => (ps, a) => ps.setBytes(i, a), Types.BINARY)
      case ld: ListData[ALG, t] @unchecked      => ???
      case ed: EitherData[ALG, a, b] @unchecked => ???
      case esd: EnumerationData[e, a] =>
        psF(i => (ps, a) => ps.setString(i, a.toString), Types.VARCHAR)
      case kvp: KvpHListValue[ALG, h, hl] @unchecked => {
        val groupF = kvpHList(kvp.kvpHList, customDbUpdateInterpreter)
        (i, k) =>
          {
            val result = groupF(i)
            val fa = (a: A) => result.actionableUpdateStatements(a.asInstanceOf[h])
            DefinitionResult(result.lastIndex, result.predefineUpdateStatements, fa)
          }
      }
      case x: HListConvert[ALG, a, al, b] @unchecked =>
        val groupF = kvpHList(x.from, customDbUpdateInterpreter)
        (i, k) =>
          {
            val result = groupF(i)
            val fa = (input: A) => result.actionableUpdateStatements(x.fAtoH(input))
            DefinitionResult(result.lastIndex, result.predefineUpdateStatements, fa)
          }
    }

}
