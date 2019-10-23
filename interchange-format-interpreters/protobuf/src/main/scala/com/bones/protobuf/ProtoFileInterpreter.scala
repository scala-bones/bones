package com.bones.protobuf

import com.bones.data.{KvpCoNil, KvpCoproduct, KvpSingleValueLeft}
import com.bones.data.Value._
import shapeless.{Coproduct, HList, Nat}

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
  case class EitherDataType(name: String, l: MessageField, r: MessageField) extends DataType

  case class OneOf(messageName: String, fields: List[MessageField]) extends DataType {
    val name = messageName.capitalize
  }

  case class MessageField(dataType: DataType,
                          required: Boolean,
                          repeated: Boolean,
                          name: String,
                          index: Int)

  /** Definitions which can be embedded in the Message */
  trait NestedType {
    def name: String
  }
  case class NestedMessage(name: String, dataTypes: Vector[MessageField])
      extends NestedType
//  case class NestedEnum(name: String, value: (String, Int)) extends NestedType
  case class Message(name: String,
                     messageFields: Vector[MessageField],
                     nestedTypes: Vector[NestedType])
      extends NestedType
//  case class OneOf(name: String, messageFields: Vector[MessageField]) extends NestedType

  def messageFieldsToProtoFile(
      fields: Vector[ProtoFileInterpreter.MessageField],
      indent: String,
      allowRequired: Boolean): String = {
    fields
      .sortBy(_.index)
      .map(field => {
        field.dataType match {
          case EitherDataType(name, left, right) =>
            s"""
               | ${indent}oneof ${name} {
               |   ${indent}  ${left.dataType.name} ${left.name} = ${left.index};
               |   ${indent}  ${right.dataType.name} ${right.name} = ${right.index};
               | ${indent}}
               | """.stripMargin('|')
          case OneOf(name, messages) =>
            val messageString = messages.map(m => s"   ${indent}  ${m.dataType.name} ${toSnake(m.name)} = ${m.index};\n               |").mkString
            s"""
               |${indent}oneof ${name} {
               |${messageString}
               |${indent}}
             """.stripMargin
          case _ => {
            val repeatedRequired =
              if (field.repeated) "repeated"
              else if (!allowRequired) ""
              else if (field.required) "required"
              else "optional"
            s"${indent}${repeatedRequired} ${field.dataType.name} ${field.name} = ${field.index};"
          }
        }
      })
      .mkString("\n")
  }

  def nestedTypeToProtoFile(
      types: Vector[ProtoFileInterpreter.NestedType]): String = {
    types.map {
      case n: NestedMessage => nestedMessageToProtoFile(n)
//      case e: NestedEnum    => ???
    } mkString ("\n")
  }

  def nestedMessageToProtoFile(message: NestedMessage): String = {
    s"""
       |  message ${message.name.capitalize} {
       |${messageFieldsToProtoFile(message.dataTypes, "    ", true)}
       |  }
       """.stripMargin

  }

  def fromSchemaToProtoFile[A](dc: BonesSchema[A]): String =
    messageToProtoFile(fromSchema(dc))

  def messageToProtoFile(message: Message): String = {
    s"""
       |message ${message.name} {
       |${messageFieldsToProtoFile(message.messageFields, "  ", true)}
       |
       |${nestedTypeToProtoFile(message.nestedTypes)}
       |}
     """.stripMargin
  }

//  def oneOfToProtoFile(oneOf: One): String = {
//    s"""
//       |  oneof ${oneOf.name} {
//       |${messageFieldsToProtoFile(oneOf.messageFields, "    ", false)}
//       |  }
//     """.stripMargin
//  }

  def fromSchema[A](dc: BonesSchema[A]): Message = {
    dc match {
      case t: HListConvert[a, al, b] =>
        val (messageFields, nestedTypes, lastIndex) = kvpHList(t.from)(0)
        Message(t.manifestOfA.runtimeClass.getSimpleName,
                messageFields,
                nestedTypes)
    }
  }

  def kvpCoproduct[C<:Coproduct](co: KvpCoproduct[C]):
    Int => (Vector[MessageField], Vector[NestedType], Int) = lastIndex => {
    co match{
      case KvpCoNil => (Vector.empty, Vector.empty, lastIndex)
      case op: KvpSingleValueLeft[l,r] => {
        val left = valueDefinition(op.kvpValue)(op.manifestH.runtimeClass.getSimpleName, lastIndex)
        val right = kvpCoproduct(op.kvpTail)(left._3)
        (left._1 +: right._1, left._2 ++ right._2, right._3)
      }
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
          kvpHList(op.tail)(r._3)
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
    : (Name, Int) => (MessageField, Vector[NestedType], Int) =
    (name, index) =>
      fgo match {
        case op: OptionalKvpValueDefinition[a] =>
          val result = valueDefinition(op.valueDefinitionOp)(name, index)
          (result._1.copy(required = false), result._2, index)
        case ob: BooleanData =>
          (MessageField(Bool, true, false, name, index), Vector.empty, index)
        case rs: StringData =>
          (MessageField(StringRequireUtf8, true, false, name, index),
           Vector.empty, index)
        case df: ShortData =>
          (MessageField(Int32, true, false, name, index), Vector.empty, index)
        case id: IntData =>
          (MessageField(Int32, true, false, name, index), Vector.empty, index)
        case ri: LongData =>
          (MessageField(Int64, true, false, name, index), Vector.empty, index)
        case uu: UuidData =>
          (MessageField(PbString, true, false, name, index), Vector.empty, index)
        case dd: LocalDateTimeData => {
          val messageFields: Vector[NestedMessage] =
            Vector(
              NestedMessage("Timestamp",
                Vector(
                  MessageField(Int64, true, false, "seconds", 1),
                  MessageField(Int64, true, false, "nanos", 2)
                )
              )
            )
          (MessageField(NestedDataType("Timestamp"), true, false, name, index), messageFields, index)
        }
        case dt: LocalDateData =>
          (MessageField(Int64, true, false, name, index), Vector.empty, index)
        case fd: FloatData =>
          (MessageField(FloatType, true, false, name, index), Vector.empty, index)
        case fd: DoubleData =>
          (MessageField(DoubleType, true, false, name, index), Vector.empty, index)
        case bd: BigDecimalData =>
          (MessageField(PbString, true, false, name, index), Vector.empty, index)
        case ba: ByteArrayData =>
          (MessageField(Bytes, true, false, name, index), Vector.empty, index)
        case ld: ListData[t] =>
          val result = valueDefinition(ld.tDefinition)(name, index)
          (result._1.copy(repeated = true), result._2, index)
        case ed: EitherData[a, b] =>
          val (messageFieldA, nestedTypesA, nextIndex) = valueDefinition(ed.definitionA)(s"${name}Left", index)
          val (messageFieldB, nestedTypesB, lastIndex) = valueDefinition(ed.definitionB)(s"${name}Right", nextIndex + 1)
          val oneOfName = toSnake(name.capitalize)
          (MessageField(EitherDataType(oneOfName, messageFieldA, messageFieldB), false, false, name, index), Vector.empty, lastIndex)
        case esd: EnumerationData[e,a] =>
          (MessageField(PbString, true, false, name, index), Vector.empty, index)
        case st: SumTypeData[a, b] =>
          (MessageField(PbString, true, false, name, index), Vector.empty, index)
        case kvp: KvpCoproductValue[c] =>
          val (fields, nestedTypes,nextIndex) = kvpCoproduct(kvp.kvpCoproduct)(index)
          val nestedMessageFields: Vector[MessageField] = nestedTypes.zipWithIndex.map(nt => MessageField(NestedDataType(nt._1.name), false, false, nt._1.name, index + nt._2))
          val name = nestedTypes.headOption.map(_.name).getOrElse("unknown")
          (MessageField(OneOf(name + "_oneof", nestedMessageFields.toList), true, false, name, nextIndex), nestedTypes, index + nestedMessageFields.length - 1)
        case kvp: KvpHListValue[h, hl] =>
          val result = kvpHList(kvp.kvpHList)(0)
          val nested = NestedMessage(name, result._1)
          (MessageField(NestedDataType(name), true, false, name, index),
           Vector(nested), index)
        case t: HListConvert[h, hl, a] =>
          val (messageFields, _, _) = kvpHList(t.from)(0)
          val nested = NestedMessage(name, messageFields)
          (MessageField(NestedDataType(name), true, false, name, index),
           Vector(nested), index)
        case co: KvpCoproductConvert[c,a] =>
          val (fields, nestedTypes, nextIndex) = kvpCoproduct(co.from)(index)
          val nestedMessageFields: Vector[MessageField] = nestedTypes.zipWithIndex.map(nt => MessageField(NestedDataType(nt._1.name), false, false, nt._1.name, index + nt._2))
          val name = nestedTypes.headOption.map(_.name).getOrElse("unknown")
          (MessageField(OneOf(name + "_oneof", nestedMessageFields.toList), true, false, name, nextIndex), nestedTypes, index + nestedMessageFields.length - 1)


    }

  def toSnake(str: String) = {
    str.replaceAll("([A-Z]+)([A-Z][a-z])", "$1_$2").replaceAll("([a-z\\d])([A-Z])", "$1_$2").toLowerCase
  }

}
