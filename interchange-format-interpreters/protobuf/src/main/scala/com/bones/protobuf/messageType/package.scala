package com.bones.protobuf

import com.bones.data.values.{
  CNilF,
  CustomStringValue,
  DefaultValues,
  JavaTimeValue,
  JavaUtilValue,
  ScalaCoreValue
}
import com.bones.protobuf.values.JavaUtilProtoFile
import shapeless.:+:

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
    index: Int
  )

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

//  val defaultProtoFileGenerators: CustomInterpreter[DefaultValues] =
//    ScalaCoreProtoFile ++
//      (CustomStringProtoFileInterpreter ++
//        (JavaTimeProtoFileInterpreter ++
//          (JavaUtilProtoFile ++ CNilProtoFileCustomInterpreterEncoder)))

  // Below is equivalent to the above.  Above compiles in 2.13, below compiles in both 2.12 and 2.13
  // start 2.12

  type JavaUtilValueCo[A] = JavaUtilValue[A] :+: CNilF[A]
  type JavaTimeValueCo[A] = JavaTimeValue[A] :+: JavaUtilValueCo[A]
  type CustomStringValueCo[A] = CustomStringValue[A] :+: JavaTimeValueCo[A]

  val defaultProtoFileGenerators: CustomInterpreter[DefaultValues] = {
    CustomInterpreter.merge[ScalaCoreValue, CustomStringValueCo](
      ScalaCoreProtoFile,
      CustomInterpreter.merge[CustomStringValue, JavaTimeValueCo](
        CustomStringProtoFileInterpreter,
        CustomInterpreter.merge[JavaTimeValue, JavaUtilValueCo](
          JavaTimeProtoFileInterpreter,
          CustomInterpreter
            .merge[JavaUtilValue, CNilF](JavaUtilProtoFile, CNilProtoFileCustomInterpreterEncoder)
        )
      )
    )
  }

  val defaultProtoFile = new ProtoFileGeneratorInterpreter[DefaultValues] {
    override def customInterpreter: CustomInterpreter[DefaultValues] = defaultProtoFileGenerators
  }
}
