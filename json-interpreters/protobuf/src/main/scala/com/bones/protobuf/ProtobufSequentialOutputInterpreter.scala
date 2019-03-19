package com.bones.protobuf

import java.io.IOException
import java.time.ZonedDateTime
import java.util.UUID

import cats.Applicative
import cats.data.{NonEmptyList, Validated}
import com.bones.data.Error.ExtractionError
import com.bones.data.Value._
import com.google.protobuf.CodedOutputStream
import shapeless.{::, HList, HNil, Nat}
import cats.implicits._




object ProtobufSequentialOutputInterpreter {


  type Path = Vector[String]
  type FieldNumber = Int
  type EncodeToProto[A] = FieldNumber => (A, CodedOutputStream) => Either[NonEmptyList[IOException], CodedOutputStream]
  type EncodeGroupToProto[H<:HList] = FieldNumber => (H, CodedOutputStream) => Either[NonEmptyList[IOException], CodedOutputStream]

  def dataClass[A](dc: DataClass[A]): EncodeToProto[A] = {
    val result = dc match {
      case t: XMapData[a, al, b] => (fieldNumber: FieldNumber) =>
        val kvp = kvpGroup(t.from)(fieldNumber)
        (input: A, outputStream: CodedOutputStream) => {
          val hlist = t.fba(input.asInstanceOf[b])
          kvp(hlist,outputStream)
        }
      case o: OptionalDataClass[a] => ???
      case ld: XMapListData[b] => ???
    }
    result.asInstanceOf[EncodeToProto[A]]
  }

  def kvpGroup[H<:HList,HL<:Nat](group: KvpGroup[H,HL]): EncodeGroupToProto[H] = {
    group match {
      case KvpNil => (fieldNumber: FieldNumber) => (_:HNil,cis:CodedOutputStream) => Right(cis)
      case op: KvpSingleValueHead[h, t, tl, o] => (fieldNumber: FieldNumber) =>
        val headF = valueDefinition(op.fieldDefinition.op)(fieldNumber)
        val tailF = kvpGroup(op.tail)(fieldNumber + 1)
        (input: o, outputStream: CodedOutputStream) => {
          val headResult = headF(input.head, outputStream)
          val tailResult = tailF(input.tail, outputStream)
          Applicative[({type AL[AA] = Either[NonEmptyList[IOException], AA]})#AL]
            .map2(headResult, tailResult)((l1: CodedOutputStream, l2: CodedOutputStream) => {
              l2
            })
        }
      case op: KvpGroupHead[a, al, h, hl, t, tl] => (fieldNumber: FieldNumber) =>
        val headF = kvpGroup(op.head)(fieldNumber)
        val tailF = kvpGroup(op.tail)(fieldNumber)
        (input: H, outputStream: CodedOutputStream) => {
          val cast = input.asInstanceOf[h :: t]
          val headResult = headF(cast.head, outputStream)
          val tailResult = tailF(cast.tail, outputStream)
          Applicative[({type AL[AA] = Either[NonEmptyList[IOException], AA]})#AL]
            .map2(headResult, tailResult)((l1: CodedOutputStream, l2: CodedOutputStream) => {
              l2
            })
        }

      case op: KvpDataClassHead[h,t,tl,out] => (fieldNumber: FieldNumber) =>
        val headF = dataClass(op.dataClass)(fieldNumber)
        val tailF = kvpGroup(op.tail)(fieldNumber)
        (input: out, outputStream: CodedOutputStream) => {
//          val cast = input.asInstanceOf[h :: t]
          val headResult = headF(input.head, outputStream)
          val tailResult = tailF(input.tail, outputStream)
          Applicative[({type AL[AA] = Either[NonEmptyList[IOException], AA]})#AL]
            .map2(headResult, tailResult)((l1: CodedOutputStream, l2: CodedOutputStream) => {
              l2
            })
        }
      case op: OptionalKvpGroup[h,hl] => (fieldNumber: FieldNumber) =>
        val kvpGroupF = kvpGroup(op.kvpGroup)(fieldNumber)
        (input: Option[h] :: HNil, outputStream: CodedOutputStream) => {
          val cast = input.asInstanceOf[Option[h] :: HNil]
          input.head match {
            case None => Right(outputStream)
            case Some(h) => kvpGroupF(h,outputStream)
          }
        }
    }
  }


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
        (l: List[t], outputStream: CodedOutputStream) => {
//          val cast = l.asInstanceOf[List[t]]
          val result = l.map(item => ft(item, outputStream))
          result.sequence.map(_ => outputStream)
        }
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
      case kvp: KvpValueData[a] => (fieldNumber: FieldNumber) =>
        val dc = dataClass(kvp.value)(fieldNumber)
        (a: A, outputStream: CodedOutputStream) => dc(a,outputStream)

    }
    result.asInstanceOf[EncodeToProto[A]]
  }


  private def write(f: => Unit): Either[NonEmptyList[IOException],Unit] =
    try {
      Right(f)
    } catch {
      case ex: IOException => Left(NonEmptyList.one(ex))
    }

}
