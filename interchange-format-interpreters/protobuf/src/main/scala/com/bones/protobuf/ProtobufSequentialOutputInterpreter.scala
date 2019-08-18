package com.bones.protobuf

import java.io.{ByteArrayOutputStream, IOException}
import java.time.{LocalDate, LocalDateTime, ZoneOffset}
import java.util.UUID

import cats.Applicative
import cats.data.{NonEmptyList, Validated}
import com.bones.data.Error.ExtractionError
import com.bones.data.Value._
import com.google.protobuf.CodedOutputStream
import shapeless._
import ops.hlist.IsHCons
import cats.implicits._

object ProtobufSequentialOutputInterpreter {

  type Path = Vector[String]
  type FieldNumber = Int
  type LastFieldNumber = Int
  type ComputeSize = () => Int
  type Encode =
    CodedOutputStream => Either[NonEmptyList[IOException], CodedOutputStream]
  type EncodeToProto[A] = FieldNumber => A => (ComputeSize, Encode)
  type EncodeHListToProto[H <: HList] =
    LastFieldNumber => H => (ComputeSize, Encode)

  def encodeToBytes[A](dc: BonesSchema[A]): A => Array[Byte] = dc match {
    case x: HListConvert[_, _, A] => {
      val group = kvpHList(x.from).apply(0)
      (a: A) =>
        {
          val hlist = x.fAtoH(a)
          val (_, fEncode) = group(hlist)
          val os = new ByteArrayOutputStream()
          val cos: CodedOutputStream = CodedOutputStream.newInstance(os)
          fEncode(cos)
          cos.flush()
          os.flush()
          os.close()
          os.toByteArray
        }
    }
  }

  protected def kvpHList[H <: HList, HL <: Nat](
      group: KvpHList[H, HL]): EncodeHListToProto[H] = {
    group match {
      case KvpNil =>
        (fieldNumber: FieldNumber) => (_: HNil) =>
          (() => 0, (os: CodedOutputStream) => Right(os))
      case op: KvpSingleValueHead[h, t, tl, o] =>
        (fieldNumber: FieldNumber) =>
          val headF = valueDefinition(op.fieldDefinition.op)(fieldNumber + 1)
          val tailF = kvpHList(op.tail)(fieldNumber + 1)
          (input: o) =>
            {
              val headResult = headF(input.head)
              val tailResult = tailF(input.tail)
              val fCompute: ComputeSize = () =>
                headResult._1() + tailResult._1()
              val fEncode: Encode = (outputStream: CodedOutputStream) => {
                Applicative[Either[NonEmptyList[IOException], ?]]
                  .map2(headResult._2(outputStream),
                        tailResult._2(outputStream))(
                    (l1: CodedOutputStream, l2: CodedOutputStream) => {
                      l2
                    })
              }
              (fCompute, fEncode)
            }
      case op: KvpHListHead[a, al, h, hl, t, tl] =>
        (fieldNumber: FieldNumber) =>
          implicit val split = op.split
          val headF = kvpHList(op.head)(fieldNumber)
          val tailF = kvpHList(op.tail)(fieldNumber)
          (input: H) =>
            {
              val (head,tail) = split(input)
              val headResult = headF(head)
              val tailResult = tailF(tail)
              val fCompute: ComputeSize = () =>
                headResult._1() + tailResult._1()
              val fEncode = (outputStream: CodedOutputStream) => {
                Applicative[Either[NonEmptyList[IOException], ?]]
                  .map2(headResult._2(outputStream),
                        tailResult._2(outputStream))(
                    (l1: CodedOutputStream, l2: CodedOutputStream) => {
                      l2
                    })
              }
              (fCompute, fEncode)
            }

      case op: KvpConcreteTypeHead[a, h, n, ho, ht, nt] =>
        (fieldNumber: FieldNumber) =>
          {
            val headF = kvpHList(op.hListConvert.from)(fieldNumber)
            val tailF = kvpHList(op.tail)(fieldNumber)
            implicit val hCons = op.isHCons
            (input: ho) =>
              {
                val headGroup = op.hListConvert.fAtoH(hCons.head(input))
                val headResult = headF(headGroup)
                val tailResult = tailF(hCons.tail(input))
                val fCompute: ComputeSize = () =>
                  headResult._1() + tailResult._1()
                val fEncode = (outputStream: CodedOutputStream) => {
                  Applicative[Either[NonEmptyList[IOException], ?]]
                    .map2(headResult._2(outputStream),
                          tailResult._2(outputStream))(
                      (l1: CodedOutputStream, l2: CodedOutputStream) => {
                        l2
                      })
                }
                (fCompute, fEncode)
              }
          }
    }
  }

  def valueDefinition[A](fgo: KvpValue[A]): EncodeToProto[A] = {
    fgo match {
      case op: OptionalKvpValueDefinition[a] =>
        (fieldNumber: FieldNumber) =>
          val fa = valueDefinition(op.valueDefinitionOp)(fieldNumber)
          (opt: Option[a]) => {
            val optB = opt.map(fa)
            (
              () => optB.fold(0)(_._1()),
              (outputStream: CodedOutputStream) =>
                optB.fold[Either[NonEmptyList[IOException], CodedOutputStream]](
                  Right(outputStream))(item => item._2(outputStream))
            )
          }
      case ob: BooleanData =>
        (fieldNumber: FieldNumber) => (bool: Boolean) =>
          (
            () => CodedOutputStream.computeBoolSize(fieldNumber, bool),
            write(_.writeBool(fieldNumber, bool))
          )
      case rs: StringData =>
        (fieldNumber: FieldNumber) => (str: String) =>
          (
            () => CodedOutputStream.computeStringSize(fieldNumber, str),
            write(_.writeString(fieldNumber, str))
          )
      case id: ShortData =>
        (fieldNumber: FieldNumber) => (s: Short) =>
          (
            () => CodedOutputStream.computeInt32Size(fieldNumber, s),
            write(_.writeInt32(fieldNumber, s))
          )
      case id: IntData =>
        (fieldNumber: FieldNumber) => (l: Int) =>
          (
            () => CodedOutputStream.computeInt32Size(fieldNumber, l),
            write(_.writeInt32(fieldNumber, l))
          )
      case ri: LongData =>
        (fieldNumber: FieldNumber) => (l: Long) =>
          (
            () => CodedOutputStream.computeInt64Size(fieldNumber, l),
            write(_.writeInt64(fieldNumber, l))
          )
      case uu: UuidData =>
        (fieldNumber: FieldNumber) => (u: UUID) =>
          (
            () => CodedOutputStream.computeStringSize(fieldNumber, u.toString),
            write(_.writeString(fieldNumber, u.toString))
          )
      case dd: LocalDateTimeData =>
        (fieldNumber: FieldNumber) => (d: LocalDateTime) =>
          (
            () => CodedOutputStream.computeInt64Size(fieldNumber,d.toEpochSecond(ZoneOffset.UTC)),
            write(_.writeInt64(fieldNumber, d.toEpochSecond(ZoneOffset.UTC)))
          )
      case dt: LocalDateData =>
        (fieldNumber: FieldNumber) => (d: LocalDate) =>
          (
            () => CodedOutputStream.computeInt64Size(fieldNumber, d.toEpochDay),
            write(_.writeInt64(fieldNumber, d.toEpochDay))
          )
      case fd: FloatData =>
        (fieldNumber: FieldNumber) => (f: Float) =>
          (
            () =>
              CodedOutputStream.computeDoubleSize(fieldNumber, f),
            write(_.writeDouble(fieldNumber, f))
          )
      case dd: DoubleData =>
        (fieldNumber: FieldNumber) => (d: Double) =>
          (
            () =>
              CodedOutputStream.computeDoubleSize(fieldNumber, d),
            write(_.writeDouble(fieldNumber, d))
          )
      case bd: BigDecimalData =>
        (fieldNumber: FieldNumber) => (d: BigDecimal) =>
          (
            () =>
              CodedOutputStream.computeStringSize(fieldNumber, d.toString()),
            write(_.writeString(fieldNumber, d.toString))
          )
      case ba: ByteArrayData =>
        (fieldNumber: FieldNumber) => (arr: Array[Byte]) =>
          (
            () => CodedOutputStream.computeByteArraySize(fieldNumber, arr),
            write(_.writeByteArray(fieldNumber, arr))
          )
      case ld: ListData[t] =>
        (fieldNumber: FieldNumber) =>
          val ft = valueDefinition(ld.tDefinition)(fieldNumber)
          (l: List[t]) =>
            {
              val result = l.map(item => ft(item))
              (
                () => result.map(_._1()).sum,
                write((outputStream: CodedOutputStream) =>
                  result.foreach(_._2(outputStream)))
              )
            }
      case ed: EitherData[a, b] => ??? // use oneOf
      case esd: EnumerationData[e,a] =>
        (fieldNumber: FieldNumber) => (a: A) =>
          (
            () => CodedOutputStream.computeStringSize(fieldNumber, a.toString),
            write(_.writeString(fieldNumber, a.toString))
          )
      case st: SumTypeData[a, b] =>
        (fieldNumber: FieldNumber) =>
          val enc = valueDefinition(st.from)(fieldNumber)
          (out: A) =>
            {
              val a = st.fba(out)
              enc(a)
            }
      case kvp: KvpHListValue[h, hl] =>
        (fieldNumber: FieldNumber) =>
          val enc = kvpHList(kvp.kvpHList)(0)
          (input: A) => enc(input.asInstanceOf[h])
      case x: HListConvert[h, hl, a] =>
        (fieldNumber: FieldNumber) =>
          val group = kvpHList(x.from).apply(0)
          (a: A) =>
            {
              val hlist = x.fAtoH(a)
              val (fSize, fEncode) = group(hlist)
              val encodeF: Encode = (outputStream: CodedOutputStream) => {
                outputStream.writeTag(fieldNumber, 2)
                outputStream.writeUInt32NoTag(fSize())
                fEncode(outputStream)
              }
              (fSize, encodeF)
            }

//        val dc = kvpHList(kvp.from)(0)
//        (a: A) => {
//          val hlist = kvp.fAtoH(a)
//          val (childSizeF, childEncodeF) = dc(hlist)
//          val size = childSizeF()
//          val sizeF: ComputeSize = () => size
//          val encodeF: Encode = (outputStream: CodedOutputStream) => {
//            outputStream.writeTag(fieldNumber, 2)
//            outputStream.writeUInt32NoTag(size)
//            childEncodeF(outputStream)
//          }
//          (sizeF, encodeF)
//        }

    }
  }

  private def write(f: CodedOutputStream => Unit): Encode =
    (codedOutputStream: CodedOutputStream) =>
      try {
        f(codedOutputStream)
        Right(codedOutputStream)
      } catch {
        case ex: IOException => Left(NonEmptyList.one(ex))
    }

}
