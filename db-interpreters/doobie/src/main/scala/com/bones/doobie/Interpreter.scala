package com.bones.doobie

import com.bones.data.Value._
import doobie.free.connection.ConnectionIO
import shapeless.{HList, Nat}
import Util.camelToSnake
import com.bones.data.KeyValueDefinition
import doobie.Query

class FindInterpreter[K](key: KeyValueDefinition[K]) {

  def dataClass[A](dc: BonesSchema[A]): K => ConnectionIO[Option[A]] = {
    val fields: List[String] = FieldNames.dataClass(dc)
    def tableName = TableName.getTableName(dc)
    val query = s"""select ${fields.mkString(", ")} from $tableName where ${camelToSnake(key.key)} = ?"""
//    k => Query[K, A](query).option(k)
    ???
  }

}

object TableName {
  def getTableName[B](dc: BonesSchema[B]): String = dc match {
    case t: XMapData[a, al, b] => camelToSnake(t.manifestOfA.runtimeClass.getSimpleName)
  }
}



object FieldNames {

  def dataClass[A](dc: BonesSchema[A]): List[String] =
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

}
