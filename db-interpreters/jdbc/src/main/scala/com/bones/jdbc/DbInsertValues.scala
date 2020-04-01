package com.bones.jdbc

import java.sql._
import java.time.{LocalDate, LocalDateTime, ZoneOffset}
import java.util.UUID

import com.bones.data.Error.SystemError
import com.bones.data.KeyValueDefinition.CoproductDataDefinition
import com.bones.data._
import com.bones.jdbc.DbUtil._
import com.bones.syntax.NoAlgebra
import javax.sql.DataSource
import shapeless.{HList, Nat, ::}

object DbInsertValues {

  trait CustomInterpreter[ALG[_]] {
    def insertPair[A](alg: ALG[A]): InsertPair[A]
  }

  case object NoAlgebraCustomInterpreter extends CustomInterpreter[NoAlgebra] {
    def insertPair[A](alg: NoAlgebra[A]): InsertPair[A] = sys.error("unreachable code")
  }

  type FieldName = String
  type FieldValue = String
  type Key = String
  type ColumnName = String
  type SetValue = PreparedStatement => Unit
  type SetNull = PreparedStatement => Unit
  type Index = Int
  type ID = Long
  type InsertPair[A] = Key => (Index, A) => (Index, List[(ColumnName, SetValue)])

  def insertQuery[ALG[_], A](
    bonesSchema: BonesSchema[ALG, A],
    customInterpreter: CustomInterpreter[ALG]): DataSource => A => Either[SystemError, (ID, A)] = {
    val iq = insertQueryWithConnectionCustomAlgebra(bonesSchema, customInterpreter)
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
              Left(SystemError(List.empty, ex, Some("Error retrieving connection")))
          }
        }
      }
  }

  def insertQueryWithConnection[ALG[_], A](
    bonesSchema: BonesSchema[NoAlgebra, A]): A => Connection => Either[SystemError, (ID, A)] =
    insertQueryWithConnectionCustomAlgebra(bonesSchema, NoAlgebraCustomInterpreter)

  def insertQueryWithConnectionCustomAlgebra[ALG[_], A](
    bonesSchema: BonesSchema[ALG, A],
    customInterpreter: CustomInterpreter[ALG]): A => Connection => Either[SystemError, (ID, A)] =
    bonesSchema match {
      case x: HListConvert[ALG, h, n, b] @unchecked => {
        val tableName = camelToSnake(x.manifestOfA.runtimeClass.getSimpleName)
        val updates = kvpHList(x.from, customInterpreter)
        a: A =>
          {
            val result = updates(1, x.fAtoH(a))
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
                    if (generatedKeys.next) Right((generatedKeys.getLong(1), a))
                    else throw new SQLException("Creating user failed, no ID obtained.")
                  } finally {
                    generatedKeys.close()
                  }
                } catch {
                  case e: SQLException => Left(SystemError(e, Some("SQL Statement: " + sql)))
                } finally {
                  statement.close()
                }
              }
          }
      }
    }

  def kvpHList[ALG[_], H <: HList, HL <: Nat](
    group: KvpHList[ALG, H, HL],
    customInterpreter: CustomInterpreter[ALG])
    : (Index, H) => (Index, List[(ColumnName, SetValue)]) = {
    group match {
      case nil: KvpNil[_] =>
        (i, h) =>
          (i, List.empty)
      case op: KvpSingleValueHead[ALG, h, t, tl, H] @unchecked => {
        val headF =
          determineValueDefinition(op.fieldDefinition.dataDefinition, customInterpreter)(op.fieldDefinition.key)
        val tailF = kvpHList(op.tail, customInterpreter)
        implicit val hCons = op.isHCons
        (i: Index, h: H) =>
          {
            val headResult = headF(i, h.head)
            val tailResult = tailF(headResult._1, h.tail)
            (tailResult._1, headResult._2 ::: tailResult._2)
          }
      }
      case op: KvpHListHead[ALG, a, al, h, hl, t, tl] @unchecked => {
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
      case op: KvpConcreteTypeHead[ALG, a, ht, nt] => {

        val headF = fromBonesSchema(op.bonesSchema, customInterpreter)
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

  private def fromBonesSchema[ALG[_], A](
    bonesSchema: BonesSchema[ALG, A],
    customInterpreter: CustomInterpreter[ALG])
    : (Index, A) => (Index, List[(ColumnName, SetValue)]) = {
    bonesSchema match {
      case co: KvpCoproductConvert[ALG, c, a] => ???
      case hListConvert: HListConvert[ALG, h, n, a] =>
        val f = kvpHList(hListConvert.from, customInterpreter)
        (index, a) =>
          {
            val h = hListConvert.fAtoH(a)
            f(index, h)
          }
    }
  }

  /** Create the return type for valueDefinition given the arguments */
  private def psF[A](f: (PreparedStatement, Index, A) => Unit): InsertPair[A] =
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
    customInterpreter: CustomInterpreter[ALG]): InsertPair[A] = {
    coproduct match {
      case Left(kvp)  => valueDefinition(kvp, customInterpreter)
      case Right(alg) => customInterpreter.insertPair(alg)
    }
  }

  def valueDefinition[ALG[_], A](
    fgo: KvpValue[A],
    customInterpreter: CustomInterpreter[ALG]): InsertPair[A] =
    fgo match {
      case op: OptionalKvpValueDefinition[ALG, b] @unchecked =>
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
      case ob: BooleanData =>
        psF[Boolean]((ps, i, a) => ps.setBoolean(i, a))
      case rs: StringData =>
        psF[String]((ps, i, a) => ps.setString(i, a))
      case id: ShortData =>
        psF[Short]((ps, i, a) => ps.setShort(i, a))
      case id: IntData =>
        psF[Int]((ps, i, a) => ps.setInt(i, a))
      case ri: LongData =>
        psF[Long]((ps, i, a) => ps.setLong(i, a))
      case uu: UuidData =>
        psF[UUID]((ps, i, a) => ps.setString(i, a.toString))
      case dd: LocalDateTimeData =>
        psF[LocalDateTime]((ps, i, a) =>
          ps.setDate(i, new java.sql.Date(a.toInstant(ZoneOffset.UTC).toEpochMilli)))
      case ld: LocalDateData =>
        psF[LocalDate]((ps, i, a) =>
          ps.setDate(i, new java.sql.Date(a.atStartOfDay().toEpochSecond(ZoneOffset.UTC))))
      case bd: BigDecimalData =>
        psF[BigDecimal]((ps, i, a) => ps.setBigDecimal(i, a.underlying))
      case fd: FloatData =>
        psF[Float]((ps, i, a) => ps.setFloat(i, a))
      case dd: DoubleData =>
        psF[Double]((ps, i, a) => ps.setDouble(i, a))
      case ba: ByteArrayData =>
        psF[scala.Array[Byte]]((ps, i, a) => ps.setBytes(i, a))
      case ld: ListData[ALG, t] @unchecked      => ???
      case ed: EitherData[ALG, a, b] @unchecked => ???
      case esd: EnumerationData[e, a] =>
        psF[A]((ps, i, a) => ps.setString(i, a.toString))
      case kvp: KvpHListValue[ALG, h, hl] @unchecked =>
        val groupF = kvpHList(kvp.kvpHList, customInterpreter)
        k =>
          { (index, a) =>
            {
              groupF(index, a.asInstanceOf[h])
            }
          }
      case x: HListConvert[ALG, a, al, b] @unchecked =>
        val groupF = kvpHList(x.from, customInterpreter)
        k =>
          { (index, h) =>
            {
              groupF(index, x.fAtoH(h))
            }
          }
    }

}
