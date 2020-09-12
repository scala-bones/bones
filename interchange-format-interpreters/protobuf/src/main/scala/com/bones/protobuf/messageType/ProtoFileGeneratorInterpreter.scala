package com.bones.protobuf.messageType

import com.bones.data.KvpCollection.headManifest
import com.bones.data._
import com.bones.data.template.KvpCollectionMatch
import shapeless.{::, Coproduct, HList, Nat}

object ProtoFileGeneratorInterpreter {

  def booleanMessageField(name: String, index: Int): (MessageField, Vector[NestedType], Int) =
    (MessageField(Bool, true, false, name, index), Vector.empty, index)

  def intMessageField(name: String, index: Int): (MessageField, Vector[NestedType], Int) =
    (MessageField(Int32, true, false, name, index), Vector.empty, index)

  def longMessageField(name: String, index: Int): (MessageField, Vector[NestedType], Int) =
    (MessageField(Int64, true, false, name, index), Vector.empty, index)

  def stringMessageField(name: String, index: Int): (MessageField, Vector[NestedType], Int) =
    (MessageField(StringRequireUtf8, true, false, name, index), Vector.empty, index)

  def byteArrayMessageField(name: String, index: Int): (MessageField, Vector[NestedType], Int) =
    (MessageField(Bytes, true, false, name, index), Vector.empty, index)

  def floatMessageField(name: String, index: Int): (MessageField, Vector[NestedType], Int) =
    (MessageField(FloatType, true, false, name, index), Vector.empty, index)

  def doubleMessageField(name: String, index: Int): (MessageField, Vector[NestedType], Int) =
    (MessageField(DoubleType, true, false, name, index), Vector.empty, index)

  def timestampMessageField(name: String, index: Int): (MessageField, Vector[NestedType], Int) = {
    val messageFields: Vector[NestedMessage] =
      Vector(
        NestedMessage(
          "Timestamp",
          Vector(
            MessageField(Int64, true, false, "seconds", 1),
            MessageField(Int64, true, false, "nanos", 2)
          )
        )
      )
    (MessageField(NestedDataType("Timestamp"), true, false, name, index), messageFields, index)
  }

}

/**
  * Create a Protobuf file descriptor based on the Kvp.
  */
trait ProtoFileGeneratorInterpreter[ALG[_]]
    extends KvpCollectionMatch[ALG, Index => (Vector[MessageField], Vector[NestedType], Index)] {

  def customInterpreter: CustomInterpreter[ALG]

  def messageFieldsToProtoFile(
    fields: Vector[MessageField],
    indent: String,
    allowRequired: Boolean
  ): String = {
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
            val messageString = messages
              .map(m =>
                s"   ${indent}  ${m.dataType.name} ${toSnake(m.name)} = ${m.index};\n               |")
              .mkString
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

  def nestedTypeToProtoFile(types: Vector[NestedType]): String = {
    types.map {
      case n: NestedMessage => nestedMessageToProtoFile(n)
    } mkString ("\n")
  }

  def nestedMessageToProtoFile(message: NestedMessage): String = {
    s"""
       |  message ${message.name.capitalize} {
       |${messageFieldsToProtoFile(message.dataTypes, "    ", true)}
       |  }
       """.stripMargin

  }

  def fromSchemaToProtoFile[A](
    dc: KvpCollection[ALG, A]
  ): String =
    messageToProtoFile(fromSchemaCustomAlgebra(dc))

  def messageToProtoFile(message: Message): String = {
    s"""
       |message ${message.name} {
       |${messageFieldsToProtoFile(message.messageFields, "  ", true)}
       |
       |${nestedTypeToProtoFile(message.nestedTypes)}
       |}
     """.stripMargin
  }

  def fromSchemaCustomAlgebra[A](
    dc: KvpCollection[ALG, A]
  ): Message = {
    val (messageFields, nestedTypes, _) = fromKvpCollection(dc).apply(0)
    Message(
      headManifest(dc).map(_.runtimeClass.getSimpleName).getOrElse("unknown"),
      messageFields,
      nestedTypes)
  }

  override def kvpCoproduct[C <: Coproduct](
    kvp: KvpCoproduct[ALG, C]
  ): Int => (Vector[MessageField], Vector[NestedType], Int) = index => {
    val thisIndex = index + 1
    val (nestedTypes, nextIndex) =
      eachKvpCoproduct(kvp)(thisIndex)
    val nestedMessageFields: Vector[MessageField] = nestedTypes.zipWithIndex.map(nt =>
      MessageField(NestedDataType(nt._1.name), false, false, nt._1.name, thisIndex + nt._2))
    val name = nestedTypes.headOption.map(_.name).getOrElse("unknown")
    (
      Vector(
        MessageField(
          OneOf(name + "_oneof", nestedMessageFields.toList),
          true,
          false,
          name,
          nextIndex)),
      nestedTypes,
      index + nestedMessageFields.length - 1)
  }

  private def eachKvpCoproduct[C <: Coproduct](
    co: KvpCoproduct[ALG, C]): Int => (Vector[NestedType], Int) =
    co match {
      case _: KvpCoNil[_] =>
        lastIndex =>
          (Vector.empty, lastIndex)
      case op: KvpCoproductCollectionHead[ALG, a, c, o] @unchecked => {
        val left = fromKvpCollection(op.kvpCollection)(0)
        val name = KvpCollection
          .headManifest(op.kvpCollection)
          .map(_.runtimeClass.getSimpleName)
          .getOrElse("unknown")
        val tailF = eachKvpCoproduct(op.kvpTail)
        lastIndex =>
          {
//            val left = leftF(lastIndex)
            val right = tailF(left._3)
            val allNested = NestedMessage(name, left._1) +: left._2.appendedAll(right._1)
            (allNested, right._2)
//            (left._1.appendedAll(right._1), left._2.appendedAll(right._2), right._3)
          }
      }
    }

  override def kvpNil(
    kvp: KvpNil[ALG]): Index => (Vector[MessageField], Vector[NestedType], Index) =
    (lastIndex) => (Vector.empty, Vector.empty, lastIndex)

  override def kvpSingleValueHead[H, T <: HList, TL <: Nat, O <: H :: T](
    kvp: KvpSingleValueHead[ALG, H, T, TL, O])
    : Index => (Vector[MessageField], Vector[NestedType], Index) = lastIndex => {
    val thisIndex = lastIndex + 1
    val head = kvp.head match {
      case Left(keyDef) => {
        val (messageField, nestedTypes, nextIndex) =
          determineValueDefinition(keyDef.dataDefinition)(keyDef.key, thisIndex)

        (Vector(messageField), nestedTypes, nextIndex)
      }
      case Right(collection) => fromKvpCollection(collection)(thisIndex)
    }

    val tail = fromKvpCollection(kvp.tail)(thisIndex)

    (head._1.appendedAll(tail._1), head._2.appendedAll(tail._2), tail._3)
  }

  override def kvpHListCollectionHead[
    HO <: HList,
    NO <: Nat,
    H <: HList,
    HL <: Nat,
    T <: HList,
    TL <: Nat](kvp: KvpHListCollectionHead[ALG, HO, NO, H, HL, T, TL])
    : Index => (Vector[MessageField], Vector[NestedType], Index) = lastIndex => {
    val head = fromKvpCollection(kvp.head)(lastIndex)
    val tail = fromKvpCollection(kvp.tail)(head._3)
    (head._1 ++ tail._1, head._2 ++ tail._2, tail._3)
  }

  override def kvpWrappedHList[A, H <: HList, HL <: Nat](
    wrappedHList: KvpWrappedHList[ALG, A, H, HL])
    : Index => (Vector[MessageField], Vector[NestedType], Index) = {
    fromKvpCollection(wrappedHList.wrappedEncoding)
  }

  override def kvpWrappedCoproduct[A, C <: Coproduct](
    wrappedCoproduct: KvpWrappedCoproduct[ALG, A, C])
    : Index => (Vector[MessageField], Vector[NestedType], Index) =
    fromKvpCollection(wrappedCoproduct.wrappedEncoding)

  def fromBonesSchema[A](
    bonesSchema: KvpCollectionValue[ALG, A]
  ): Int => (Vector[MessageField], Vector[NestedType], Int) = {
    fromKvpCollection(bonesSchema.kvpCollection)
  }

  def determineValueDefinition[A](
    value: Either[HigherOrderValue[ALG, A], ALG[A]]
  ): (Name, Int) => (MessageField, Vector[NestedType], Int) =
    value match {
      case Left(kvp)  => valueDefinition(kvp)
      case Right(alg) => customInterpreter.toMessageField(alg)
    }

  def valueDefinition[A](
    fgo: HigherOrderValue[ALG, A]): (Name, Int) => (MessageField, Vector[NestedType], Int) =
    fgo match {
      case op: OptionalValue[ALG, a] @unchecked =>
        (name, index) =>
          val result =
            determineValueDefinition(op.valueDefinitionOp)(name, index)
          (result._1.copy(required = false), result._2, index)
      case ld: ListData[ALG, t] @unchecked =>
        (name, index) =>
          val result = determineValueDefinition(ld.tDefinition)(name, index)
          (result._1.copy(repeated = true), result._2, index)
      case ed: EitherData[ALG, a, b] @unchecked =>
        (name, index) =>
          val (messageFieldA, nestedTypesA, nextIndex) =
            determineValueDefinition(ed.definitionA)(name, index)
          val (messageFieldB, nestedTypesB, lastIndex) = determineValueDefinition(
            ed.definitionB
          )(name, nextIndex + 1)

          //Append the type to the name so that the name is unique.
          val messageFieldFinalA =
            messageFieldA.copy(name = name + messageFieldA.dataType.name.capitalize)
          val messageFieldFinalB =
            messageFieldB.copy(name = name + messageFieldB.dataType.name.capitalize)
          val oneOfName = toSnake(name.capitalize)
          (
            MessageField(
              EitherDataType(oneOfName, messageFieldFinalA, messageFieldFinalB),
              false,
              false,
              name,
              index
            ),
            Vector.empty,
            lastIndex
          )
      case kvp: KvpCollectionValue[ALG, a] @unchecked =>
        (name, index) =>
          val result = fromKvpCollection(kvp.kvpCollection)(0)
          val nested = result._2.appended(NestedMessage(name, result._1))
          (MessageField(NestedDataType(name), true, false, name, index), nested, index)
    }

  def toSnake(str: String) = {
    str
      .replaceAll("([A-Z]+)([A-Z][a-z])", "$1_$2")
      .replaceAll("([a-z\\d])([A-Z])", "$1_$2")
      .toLowerCase
  }

}
