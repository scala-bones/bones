package com.bones.protobuf

import com.bones.data.values.DefaultValues
import com.bones.protobuf.values.JavaUtilProtoFile

package object messageType {
  type Required = Boolean
  type Repeated = Boolean
  type Name = String
  type Index = Int

  case class MessageField(
    dataType: DataType,
    required: Boolean,
    repeated: Boolean,
    name: String,
    index: Int)

  /** Definitions which can be embedded in the Message */
  trait NestedType {
    def name: String
  }
  case class NestedMessage(name: String, dataTypes: Vector[MessageField]) extends NestedType

  case class Message(
    name: String,
    messageFields: Vector[MessageField],
    nestedTypes: Vector[NestedType]
  ) extends NestedType

  object ScalaCoreProtoFile extends ScalaCoreFileInterpreter

  val defaultProtoFileGenerators: CustomInterpreter[DefaultValues] =
    ScalaCoreProtoFile ++
      (CustomStringProtoFileInterpreter ++
        (JavaTimeProtoFileInterpreter ++
          (JavaUtilProtoFile ++ CNilProtoFileCustomInterpreterEncoder)))

  val defaultProtoFile = new ProtoFileGeneratorInterpreter[DefaultValues] {
    override def customInterpreter: CustomInterpreter[DefaultValues] = defaultProtoFileGenerators
  }
}
