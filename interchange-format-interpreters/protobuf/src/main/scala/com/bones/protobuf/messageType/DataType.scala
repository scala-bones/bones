package com.bones.protobuf.messageType

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
