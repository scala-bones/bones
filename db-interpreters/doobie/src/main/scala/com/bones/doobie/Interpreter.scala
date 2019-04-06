package com.bones.doobie

import java.sql.{ResultSet, SQLException}
import java.util.{Calendar, TimeZone}

import com.bones.data.Value._
import doobie.free.connection.ConnectionIO
import shapeless.{HList, HNil, Nat}
import DoobieUtil.camelToSnake
import cats.data.NonEmptyList
import com.bones.Util
import com.bones.data.Error.{ExtractionError, RequiredData, SystemError}
import com.bones.Util._
import doobie.Query
import doobie.util.query.Query0

import scala.util.Try

object FindInterpreter {

  val utcCalendar = Calendar.getInstance(TimeZone.getTimeZone("UTC"))
  type FieldName = String
  type Path = List[String]

//  def fromSchema[A](dc: BonesSchema[A]): Int => ConnectionIO[Option[A]] = {
//    val fields: List[String] = FieldNames.fromSchema(dc)
//    def tableName = TableName.getTableName(dc)
//    val query = s"""select ${fields.mkString(", ")} from $tableName where id =  = ?"""
//    (id: Int) => Query0[A](query).option(id)
//  }




}

object TableName {
  def getTableName[B](dc: BonesSchema[B]): String = dc match {
    case t: XMapData[a, al, b] => camelToSnake(t.manifestOfA.runtimeClass.getSimpleName)
  }
}



object FieldNames {

  def fromSchema[A](dc: BonesSchema[A]): List[String] =
    dc match {
      case t: XMapData[a, al, b] => kvpGroup(t.from)
    }

  def kvpGroup[H<:HList,HL<:Nat](group: KvpGroup[H,HL]): List[String] =
    group match {
      case KvpNil => List.empty
      case op: KvpSingleValueHead[h, t, tl, a] => List(camelToSnake(op.fieldDefinition.key)) ::: kvpGroup(op.tail)
      case op: KvpGroupHead[a, al, h, hl, t, tl] =>
        kvpGroup(op.head) ::: kvpGroup(op.tail)
      case op: OptionalKvpGroup[h,hl] =>
        kvpGroup(op.kvpGroup)
    }


  def valueDefinition[A](fgo: ValueDefinitionOp[A]): List[String] =
    fgo match {
      case op: OptionalValueDefinition[a] => valueDefinition(op.valueDefinitionOp)
      case ob: BooleanData => List.empty
      case rs: StringData => List.empty
      case ri: LongData => List.empty
      case uu: UuidData => List.empty
      case dd: DateTimeData => List.empty
      case bd: BigDecimalData => List.empty
      case ld: ListData[t] => List.empty
      case ed: EitherData[a,b] => List.empty
      case esd: EnumerationStringData[a] => List.empty
      case esd: EnumStringData[a] => List.empty
      case kvp: KvpGroupData[h,hl] => kvpGroup(kvp.kvpGroup)
      case x: XMapData[_,_,_] => kvpGroup(x.from)
    }

}

object X {
//  import doobie._
//  import doobie.implicits._
//  import cats._
//  import cats.data._
//  import cats.effect.IO
//  import cats.implicits._
//  import scala.concurrent.ExecutionContext
//
//  {
//    sql"select name from country"
//      .query[String]    // Query0[String]
//      .to[List]         // ConnectionIO[List[String]]
//      .transact(xa)     // IO[List[String]]
//      .unsafeRunSync    // List[String]
//      .take(5)          // List[String]
//      .foreach(println) // Unit
//  }
}
