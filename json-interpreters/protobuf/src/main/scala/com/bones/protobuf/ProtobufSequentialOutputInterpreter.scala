package com.bones.protobuf

import java.io.IOException
import java.time.ZonedDateTime
import java.util.UUID

import com.bones.data.Value._
import com.google.protobuf.CodedOutputStream
import shapeless.{HList, Nat}


class ProtobufSequentialOutputInterpreter {


  type Path = Vector[String]
  type FieldNumber = Int
  type EncodeToProto[A] = FieldNumber => (A, CodedOutputStream) => Either[IOException, CodedOutputStream]
  type EncodeGroupToProto[H<:HList] = FieldNumber => (H, CodedOutputStream) => Either[IOException, CodedOutputStream]

  def kvpGroup[H<:HList,HL<:Nat](group: KvpGroup[H,HL]): EncodeGroupToProto[H] = ???


  def valueDefinition[A](fgo: ValueDefinitionOp[A]): EncodeToProto[A] = {
    val result = fgo match {
      case op: OptionalValueDefinition[a] => (fieldNumber: FieldNumber) =>
        val fa = valueDefinition(op.valueDefinitionOp)(fieldNumber)
        (opt: Option[a], outputStream: CodedOutputStream) => opt.foreach(item => fa(item, outputStream))
      case ob: BooleanData => (fieldNumber: FieldNumber) =>
        (bool: Boolean, outputStream: CodedOutputStream) => write(outputStream.writeBool(fieldNumber, bool))
      case rs: StringData => (fieldNumber: FieldNumber) =>
        (str: String, outputStream: CodedOutputStream) => write(outputStream.writeString(fieldNumber, str))
      case ri: LongData => (fieldNumber: FieldNumber) =>
        (l: Long, outputStream: CodedOutputStream) => write(outputStream.writeInt64(fieldNumber, l))
      case uu: UuidData => (fieldNumber: FieldNumber) =>
        (u: UUID, outputStream: CodedOutputStream) =>  write(outputStream.writeString(fieldNumber, u.toString))
      case dd: DateTimeData => (fieldNumber: FieldNumber) =>
        (d: ZonedDateTime, outputStream: CodedOutputStream) =>
          write(outputStream.writeString(fieldNumber, dd.dateFormat.format(d)))
      case bd: BigDecimalData => (fieldNumber: FieldNumber) =>
        (d: BigDecimal, outputStream: CodedOutputStream) =>
          write(outputStream.writeString(fieldNumber, d.toString))
      case ld: ListData[t] => (fieldNumber: FieldNumber) =>
        val ft = valueDefinition(ld.tDefinition)(fieldNumber)
        (l: List[t], outputStream: CodedOutputStream) =>
          l.foreach(item => ft(item, outputStream))
      case ed: EitherData[a, b] => ??? // use oneOf
      case esd: EnumerationStringData[a] => (fieldNumber: FieldNumber) =>
        (a: A, outputStream: CodedOutputStream) =>
          write(outputStream.writeString(fieldNumber, a.toString))
      case esd: EnumStringData[a] => (fieldNumber: FieldNumber) =>
        (a: a, outputStream: CodedOutputStream) =>
          write(outputStream.writeString(fieldNumber, a.toString))
      case kvp: KvpGroupData[h, hl] => (fieldNumber: FieldNumber) =>
        val enc = kvpGroup(kvp.kvpGroup)(fieldNumber)
        (h: h, outputStream: CodedOutputStream) => enc(h,outputStream)

    }
    result.asInstanceOf[EncodeToProto[A]]
  }


  private def write(f: => Unit): Either[IOException,Unit] =
    try {
      Right(f)
    } catch {
      case ex: IOException => Left(ex)
    }

}
