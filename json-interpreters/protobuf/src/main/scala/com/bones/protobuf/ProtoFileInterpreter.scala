package com.bones.protobuf

import com.bones.data.Value._
import shapeless.ops.hlist.Repeat
import shapeless.{HList, Nat}

object ProtoFileInterpreter {

  type Required = Boolean
  type Repeated = Boolean
  type Name = String
  type Index = Int

  /** Types that can be in a Message */
  sealed trait DataType {
    def name: String
  }
  case object Int32 extends DataType {
    val name = "int32"
  }
  case object Bool extends DataType {
    val name = "bool"
  }
  case object StringRequireUtf8 extends DataType {
    val name = "string"
  }
  case object Int64 extends DataType {
    val name = "int64"
  }
  case object PbString extends DataType {
    val name = "string"
  }
  case class NestedDataType(messageName: String) extends DataType {
    val name = messageName.capitalize
  }

  case class MessageField(dataType: DataType, required: Boolean, repeated: Boolean, name: String, index: Int)

  /** Definitions which can be embedded in the Message */
  trait NestedType
  case class NestedMessage(name: String, dataTypes: Vector[MessageField]) extends NestedType
  case class NestedEnum(name: String, value: (String, Int)) extends NestedType

  /** Container for a Proto Message */
  case class Message(name: String, messageFields: Vector[MessageField], nestedTypes: Vector[NestedType])

  def messageFieldsToProtoFile(fields: Vector[ProtoFileInterpreter.MessageField], indent: String): String = {
    fields.sortBy(_.index).map(field => {
      val repeatedRequired =
        if (field.repeated) "repeated"
        else if (field.required) "required"
        else "optional"
      s"${indent}${repeatedRequired} ${field.dataType.name} ${field.name} = ${field.index};"
    }).mkString("\n")
  }

  def nestedTypeToProtoFile(types: Vector[ProtoFileInterpreter.NestedType]): String = {
    types.map {
      case n: NestedMessage => nestedMessageToProtoFile(n)
      case e: NestedEnum => ???
    } mkString("\n")
  }

  def nestedMessageToProtoFile(message: NestedMessage): String = {
    s"""
       |  message ${message.name.capitalize} {
       |${messageFieldsToProtoFile(message.dataTypes, "    ")}
       |  }
       """.stripMargin

  }

  def messageToProtoFile(message: Message) : String = {
    s"""
       |message ${message.name} {
       |${messageFieldsToProtoFile(message.messageFields, "  ")}
       |
       |${nestedTypeToProtoFile(message.nestedTypes)}
       |}
     """.stripMargin
  }

  def dataClass[A](dc: DataClass[A]): Message = {
    dc match {
      case t: XMapData[a, al, b] =>
        val (messageFields, nestedTypes, lastIndex) = kvpGroup(t.from)(0)
        Message(t.manifestOfA.runtimeClass.getSimpleName, messageFields, nestedTypes)

      case o: OptionalDataClass[a] =>
        dataClass(o.value)
      case ld: XMapListData[b] =>
        dataClass(ld.value)
    }
  }

  private def nestedDataClass[A](dc: DataClass[A]): Int => (Vector[MessageField], Vector[NestedType], Int) = lastIndex => {
    dc match {
      case t: XMapData[a, al, b] =>
        kvpGroup(t.from)(lastIndex)
      case o: OptionalDataClass[a] =>
        val x = nestedDataClass(o.value)(lastIndex)
        (x._1, x._2, x._3)
      case ld: XMapListData[b] =>
        val x = nestedDataClass(ld.value)(lastIndex)
        (x._1, x._2, x._3)
    }
  }

  def kvpGroup[H<:HList,HL<:Nat](group: KvpGroup[H,HL]): Int => (Vector[MessageField], Vector[NestedType], Int) = lastIndex => {
    group match {
      case KvpNil => (Vector.empty, Vector.empty, lastIndex)
      case op: KvpSingleValueHead[h, t, tl, a] => {
        val thisIndex = lastIndex + 1
        val r = valueDefinition(op.fieldDefinition.op)(op.fieldDefinition.key, thisIndex)
        val (messageFields, nestedTypes, lastUsedIndex) = kvpGroup(op.tail)(thisIndex)
        (messageFields :+ r._1, r._2 ++ nestedTypes, lastUsedIndex)
      }
      case op: KvpGroupHead[a, al, h, hl, t, tl] =>
        val head = kvpGroup(op.head)(lastIndex)
        val tail = kvpGroup(op.tail)(head._3)
        (head._1 ++ tail._1, head._2 ++ tail._2, tail._3)
      case op: KvpDataClassHead[h,t,tl,out] =>
        val head = nestedDataClass(op.dataClass)(lastIndex)
        val tail = kvpGroup(op.tail)(head._3)
        (head._1 ++ tail._1, head._2 ++ tail._2, lastIndex)
      case op: OptionalKvpGroup[h,hl] =>
        kvpGroup(op.kvpGroup)(lastIndex)
    }
  }

  def valueDefinition[A](fgo: ValueDefinitionOp[A]): (Name, Int) => (MessageField, Vector[NestedType]) = (name, index) =>
    fgo match {
      case op: OptionalValueDefinition[a] =>
        val result = valueDefinition(op.valueDefinitionOp)(name, index)
        (result._1.copy(required = false), result._2)
      case ob: BooleanData => (MessageField(Bool, true, false, name, index), Vector.empty)
      case rs: StringData => (MessageField(StringRequireUtf8, true, false, name, index), Vector.empty)
      case ri: LongData => (MessageField(Int64, true, false, name, index), Vector.empty)
      case uu: UuidData => (MessageField(PbString, true, false, name, index), Vector.empty)
      case dd: DateTimeData => (MessageField(PbString, true, false, name, index), Vector.empty)
      case bd: BigDecimalData => (MessageField(PbString, true, false, name, index), Vector.empty)
      case ld: ListData[t] =>
        val result = valueDefinition(ld.tDefinition)(name, index)
        (result._1.copy(repeated = true), result._2)
      case ed: EitherData[a,b] => ???
      case esd: EnumerationStringData[a] => (MessageField(PbString, true, false, name, index), Vector.empty)
      case esd: EnumStringData[a] => (MessageField(PbString, true, false, name, index), Vector.empty)
      case st: SumTypeData[A,a] =>
        (MessageField(PbString, true, false, name, index), Vector.empty)
      case kvp: KvpGroupData[h,hl] =>
        val result = kvpGroup(kvp.kvpGroup)(0)
        val nested = NestedMessage(name, result._1)
        (MessageField(NestedDataType(name), true, false, name, index), Vector(nested))
      case k: KvpValueData[a] =>
        val result = nestedDataClass(k.value)(0)
        val nested = NestedMessage(name, result._1)
        (MessageField(NestedDataType(name), true, false, name, index), Vector(nested))

    }

}