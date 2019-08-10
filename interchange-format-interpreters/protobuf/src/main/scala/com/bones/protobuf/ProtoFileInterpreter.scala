package com.bones.protobuf

import com.bones.data.Value._
import shapeless.ops.hlist.Repeat
import shapeless.{HList, Nat}

/**
  * Create a Protobuf file descriptor based on the Kvp.
  */
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
  case object FloatType extends DataType {
    val name = "float"
  }
  case object DoubleType extends DataType {
    val name = "double"
  }
  case object PbString extends DataType {
    val name = "string"
  }
  case object Bytes extends DataType {
    val name = "bytes"
  }
  case class NestedDataType(messageName: String) extends DataType {
    val name = messageName.capitalize
  }

  case class MessageField(dataType: DataType,
                          required: Boolean,
                          repeated: Boolean,
                          name: String,
                          index: Int)

  /** Definitions which can be embedded in the Message */
  trait NestedType
  case class NestedMessage(name: String, dataTypes: Vector[MessageField])
      extends NestedType
  case class NestedEnum(name: String, value: (String, Int)) extends NestedType
  case class Message(name: String,
                     messageFields: Vector[MessageField],
                     nestedTypes: Vector[NestedType])
      extends NestedType

  def messageFieldsToProtoFile(
      fields: Vector[ProtoFileInterpreter.MessageField],
      indent: String): String = {
    fields
      .sortBy(_.index)
      .map(field => {
        val repeatedRequired =
          if (field.repeated) "repeated"
          else if (field.required) "required"
          else "optional"
        s"${indent}${repeatedRequired} ${field.dataType.name} ${field.name} = ${field.index};"
      })
      .mkString("\n")
  }

  def nestedTypeToProtoFile(
      types: Vector[ProtoFileInterpreter.NestedType]): String = {
    types.map {
      case n: NestedMessage => nestedMessageToProtoFile(n)
      case e: NestedEnum    => ???
    } mkString ("\n")
  }

  def nestedMessageToProtoFile(message: NestedMessage): String = {
    s"""
       |  message ${message.name.capitalize} {
       |${messageFieldsToProtoFile(message.dataTypes, "    ")}
       |  }
       """.stripMargin

  }

  def fromSchemaToProtoFile[A](dc: BonesSchema[A]): String =
    messageToProtoFile(fromSchema(dc))

  def messageToProtoFile(message: Message): String = {
    s"""
       |message ${message.name} {
       |${messageFieldsToProtoFile(message.messageFields, "  ")}
       |
       |${nestedTypeToProtoFile(message.nestedTypes)}
       |}
     """.stripMargin
  }

  def fromSchema[A](dc: BonesSchema[A]): Message = {
    dc match {
      case t: HListConvert[a, al, b] =>
        val (messageFields, nestedTypes, lastIndex) = kvpHList(t.from)(0)
        Message(t.manifestOfA.runtimeClass.getSimpleName,
                messageFields,
                nestedTypes)
    }
  }

  def kvpHList[H <: HList, HL <: Nat](group: KvpHList[H, HL])
    : Int => (Vector[MessageField], Vector[NestedType], Int) = lastIndex => {
    group match {
      case KvpNil => (Vector.empty, Vector.empty, lastIndex)
      case op: KvpSingleValueHead[h, t, tl, a] => {
        val thisIndex = lastIndex + 1
        val r = valueDefinition(op.fieldDefinition.op)(op.fieldDefinition.key,
                                                       thisIndex)
        val (messageFields, nestedTypes, lastUsedIndex) =
          kvpHList(op.tail)(thisIndex)
        (messageFields :+ r._1, r._2 ++ nestedTypes, lastUsedIndex)
      }
      case op: KvpConcreteTypeHead[a, ht, nt, ho, hx, nx] =>
        val head = kvpHList(op.hListConvert.from)(lastIndex)
        val tail = kvpHList(op.tail)(head._3)
        (head._1 ++ tail._1, head._2 ++ tail._2, tail._3)
      case op: KvpHListHead[a, al, h, hl, t, tl] =>
        val head = kvpHList(op.head)(lastIndex)
        val tail = kvpHList(op.tail)(head._3)
        (head._1 ++ tail._1, head._2 ++ tail._2, tail._3)
    }
  }

  def valueDefinition[A](fgo: KvpValue[A])
    : (Name, Int) => (MessageField, Vector[NestedType]) =
    (name, index) =>
      fgo match {
        case op: OptionalKvpValueDefinition[a] =>
          val result = valueDefinition(op.valueDefinitionOp)(name, index)
          (result._1.copy(required = false), result._2)
        case ob: BooleanData =>
          (MessageField(Bool, true, false, name, index), Vector.empty)
        case rs: StringData =>
          (MessageField(StringRequireUtf8, true, false, name, index),
           Vector.empty)
        case id: IntData =>
          (MessageField(Int32, true, false, name, index), Vector.empty)
        case ri: LongData =>
          (MessageField(Int64, true, false, name, index), Vector.empty)
        case uu: UuidData =>
          (MessageField(PbString, true, false, name, index), Vector.empty)
        case dd: DateTimeData =>
          (MessageField(Int64, true, false, name, index), Vector.empty)
        case dt: LocalDateData =>
          (MessageField(Int64, true, false, name, index), Vector.empty)
        case fd: FloatData =>
          (MessageField(FloatType, true, false, name, index), Vector.empty)
        case fd: DoubleData =>
          (MessageField(DoubleType, true, false, name, index), Vector.empty)
        case bd: BigDecimalData =>
          (MessageField(PbString, true, false, name, index), Vector.empty)
        case ba: ByteArrayData =>
          (MessageField(Bytes, true, false, name, index), Vector.empty)
        case ld: ListData[t] =>
          val result = valueDefinition(ld.tDefinition)(name, index)
          (result._1.copy(repeated = true), result._2)
        case ed: EitherData[a, b] => ??? //use one of
        case esd: EnumerationStringData[a] =>
          (MessageField(PbString, true, false, name, index), Vector.empty)
        case st: SumTypeData[a, b] =>
          (MessageField(PbString, true, false, name, index), Vector.empty)
        case kvp: KvpHListValue[h, hl] =>
          val result = kvpHList(kvp.kvpHList)(0)
          val nested = NestedMessage(name, result._1)
          (MessageField(NestedDataType(name), true, false, name, index),
           Vector(nested))
        case t: HListConvert[h, hl, a] =>
          val (messageFields, _, _) = kvpHList(t.from)(0)
          val nested = NestedMessage(name, messageFields)
          (MessageField(NestedDataType(name), true, false, name, index),
           Vector(nested))

    }

}
