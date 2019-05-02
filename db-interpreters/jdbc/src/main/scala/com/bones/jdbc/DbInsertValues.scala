package com.bones.jdbc

import java.sql._
import java.time.ZonedDateTime
import java.util.UUID

import com.bones.data.Value._
import shapeless.{::, HList, HNil, Nat}
import DbUtil._
import com.bones.crud.WithId
import com.bones.data.Error.SystemError
import javax.sql.DataSource

object DbInsertValues {

  type FieldName = String
  type FieldValue = String
  type Key = String
  type ColumnName = String
  type SetValue = PreparedStatement => Unit
  type SetNull = PreparedStatement => Unit
  type Index = Int
  type ID = Long
  type InsertPair[A] = Key => (Index, A) => (Index, List[(ColumnName, SetValue)])

  def insertQuery[A](bonesSchema: BonesSchema[A]): DataSource => A => Either[SystemError, WithId[ID,A]] = {
    val iq = insertQueryWithConnection(bonesSchema)
    ds => {
      a => {
        try {
          val con = ds.getConnection
          val result = iq(a)(con)
          con.close()
          result
        } catch {
          case ex: SQLException => Left(SystemError(List.empty, ex, Some("Error retrieving connection")))
        }
      }
    }
  }

  def insertQueryWithConnection[A](bonesSchema: BonesSchema[A]): A => Connection => Either[SystemError, WithId[Long,A] ] =
    bonesSchema match {
      case x: XMapData[h,n,b] => {
        val tableName = camelToSnake(x.manifestOfA.runtimeClass.getSimpleName)
        val updates = valueDefinition(x)
        a: A => {
          val result = updates("")(1,a)
          val sql = s"""insert into $tableName ( ${result._2.map(_._1).mkString(",")} ) values ( ${result._2.map(_ => "?").mkString(",")}  )"""
          con: Connection => {
            val statement = con.prepareStatement(sql, Statement.RETURN_GENERATED_KEYS)
            try {
              result._2.map(_._2).foreach(f => f(statement))
              statement.executeUpdate()
              val generatedKeys = statement.getGeneratedKeys
              try {
                if (generatedKeys.next) Right( WithId(generatedKeys.getLong(1), a ) )
                else throw new SQLException("Creating user failed, no ID obtained.")
              } finally {
                generatedKeys.close()
              }
            } catch {
              case e: SQLException => Left(SystemError(List.empty, e,Some("SQL Statement: " + sql)))
            } finally {
              statement.close()
            }
          }
        }
      }
    }

  def kvpHList[H<:HList,HL<:Nat](group: KvpHList[H,HL]): (Index, H) => (Index, List[(ColumnName, SetValue)]) = {
    group match {
      case KvpNil => (i,h) => (i, List.empty)
      case op: KvpSingleValueHead[h, t, tl, a] => {
        val headF = valueDefinition(op.fieldDefinition.op)(op.fieldDefinition.key)
        val tailF = kvpHList(op.tail)
        (i: Index,h:H) => {
          val headResult = headF(i,h.head)
          val tailResult = tailF(headResult._1,h.tail)
          (tailResult._1, headResult._2 ::: tailResult._2)
        }
      }
      case op: KvpHListHead[a, al, h, hl, t, tl] => {
        val headF = kvpHList(op.head)
        val tailF = kvpHList(op.tail)
        (i:Index,h:H) => {
          val hSplit = op.split(h)
          val headList = headF(i,hSplit._1)
          val tailList = tailF(headList._1, hSplit._2)
          (tailList._1, headList._2 ::: tailList._2)
        }
      }
      case op: KvpXMapDataHead[a,ht,nt,ho,xl,xll] => {
        val headF = kvpHList(op.xmapData.from)
        val tailF = kvpHList(op.tail)
        (i:Index,h:H) => {
//          val hSplit = op.split(h)
          val headList = headF(i,op.xmapData.fba(h.head))
          val tailList = tailF(headList._1, h.tail)
          (tailList._1, headList._2 ::: tailList._2)
        }
      }
    }
  }

  /** Create the return type for valueDefinition given the arguments */
  private def psF[A](f: (PreparedStatement,  Index, A) => Unit) : InsertPair[A] =
    key => {
      val columnName = camelToSnake(key)
      (index: Index, a: A) => {
        val setValue: SetValue = ps => {
          f(ps, index, a)
        }
        (index + 1, List( (columnName, setValue) ))
      }
    }

  def valueDefinition[A](fgo: ValueDefinitionOp[A]): InsertPair[A] =
    fgo match {
      case op: OptionalValueDefinition[b] =>
        val valueF = valueDefinition(op.valueDefinitionOp)
        key => {
          (index: Index, a: A) => {
            a match {
              case Some(b) => valueF(key)(index,b)
              case None => (index, List.empty)
            }
          }
        }
      case ob: BooleanData =>
        psF[Boolean]( (ps,i,a) => ps.setBoolean(i,a))
      case rs: StringData =>
        psF[String]( (ps,i,a) => ps.setString(i,a))
      case ri: LongData =>
        psF[Long]( (ps,i,a) => ps.setLong(i,a))
      case uu: UuidData =>
        psF[UUID]( (ps,i,a) => ps.setString(i,a.toString))
      case dd: DateTimeData =>
        psF[ZonedDateTime]( (ps, i ,a) => ps.setDate(i, new java.sql.Date(a.toInstant.toEpochMilli)))
      case bd: BigDecimalData =>
        psF[BigDecimal]( (ps,i, a) => ps.setBigDecimal(i,a.underlying))
      case ba: ByteArrayData =>
        psF[scala.Array[Byte]]( (ps,i,a) => ps.setBytes(i, a))
      case ld: ListData[t] => ???
      case ed: EitherData[a,b] => ???
      case esd: EnumerationStringData[a] =>
        psF[A]( (ps,i,a) => ps.setString(i,a.toString))
      case kvp: KvpHListValue[h,hl] =>
        val groupF = kvpHList(kvp.kvpHList)
        k => {
          (index, a) => {
            groupF(index,a.asInstanceOf[h])
          }
        }
      case x: XMapData[a,al,b] =>
        val groupF = kvpHList(x.from)
        k => {
          (index, h) => {
            groupF(index,x.fba(h))
          }
        }
      case s: SumTypeData[a,b] =>
        val groupF = valueDefinition(s.from)
        k => {
          val groupK = groupF(k)
          (index, a) => {
            groupK(index,s.fba(a))
          }
        }
    }

}
