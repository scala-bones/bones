package com.bones.doobie

import java.sql.PreparedStatement
import java.time.ZonedDateTime

import com.bones.data.Value._
import shapeless.{::, HList, HNil, Nat}
import DoobieUtil._

/** insert into table (field1, field2, field3) values (:value1, :value2, :value3) */
object DbInsertValues {

  type FieldName = String
  type FieldValue = String
  type Key = String
  type UpdateString = String
  type SetValue = PreparedStatement => Unit
  type Index = Int

  def kvpGroup[H<:HList,HL<:Nat](group: KvpGroup[H,HL]): Index => H => (Index, List[(UpdateString, SetValue)]) = {
    group match {
      case KvpNil => i => (h: HNil) => (1, List.empty)
      case op: KvpSingleValueHead[h, t, tl, a] => {
        val headF = valueDefinition(op.fieldDefinition.op)
        val tailF = kvpGroup(op.tail)
        (i: Index) => {
          val headG = headF(i,op.fieldDefinition.key)
          (h: H) => {
            val headList = headG(h.head)
            val tailList = tailF(headList._1)(h.tail)
            (tailList._1, (headList._2) ::: tailList._2)
          }
        }
      }
      case op: KvpGroupHead[a, al, h, hl, t, tl] => {
        val headF = kvpGroup(op.head)
        val tailF = kvpGroup(op.tail)
        (i: Index) => {
          val headG = headF(i)
          (h: H) => {
            val cast = h.asInstanceOf[h :: t]
            val headList = headG(cast.head)
            val tailList = tailF(headList._1)(cast.tail)
            (tailList._1, headList._2 ::: tailList._2)
          }
        }
      }
      case op: OptionalKvpGroup[h,hl] => ???
    }
  }

  /** Create the return type for valueDefinition given the arguments */
  private def psF[A](f: Index => (PreparedStatement,  A) => Unit) : (Index, Key) => A => (Index, List[(UpdateString, SetValue)]) =
  (index, key) => {
    val updateString = s"set ${key} = ?"
    val fI = f(index)
    a => {
      val psF: SetValue = ps => fI(ps, a)
      (index + 1, List( (updateString, psF) ))
    }
  }

  def valueDefinition[A](fgo: ValueDefinitionOp[A]): (Index, Key) =>  A => (Index, List[(UpdateString, SetValue)]) =
    fgo match {
      case op: OptionalValueDefinition[a] => valueDefinition(op)
      case ob: BooleanData => psF(i => (ps,a) => ps.setBoolean(i, a))
      case rs: StringData => psF(i => (ps,a) => ps.setString(i,a))
      case ri: LongData => psF(i => (ps,a) => ps.setLong(i,a))
      case uu: UuidData => psF(i => (ps,a) => ps.setString(i,a.toString))
      case dd: DateTimeData =>
        psF((i: Index) => (ps: PreparedStatement ,a: ZonedDateTime) => ps.setDate(i, new java.sql.Date(a.toInstant.toEpochMilli)))
      case bd: BigDecimalData =>
        psF[BigDecimal](i => (ps,a) => ps.setBigDecimal(i,a.underlying))
      case ld: ListData[t] => ???
      case ed: EitherData[a,b] => ???
      case esd: EnumerationStringData[a] => ???
      case esd: EnumStringData[a] => ???
      case kvp: KvpGroupData[h,hl] => {
        val groupF = kvpGroup(kvp.kvpGroup)
        (i,k) => {
          val groupI = groupF(i)
          a => groupI(a.asInstanceOf[h])
        }
      }
      case x: XMapData[a,al,b] =>
        val groupF = kvpGroup(x.from)
        (i,k) => {
          val groupI = groupF(i)
          (input: b) => groupI(x.fba(input))
        }
    }

}
