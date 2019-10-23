package com.bones.protobuf

import java.io.{ByteArrayOutputStream, IOException}
import java.time.{LocalDate, LocalDateTime, ZoneOffset}
import java.util.UUID

import cats.Applicative
import cats.data.NonEmptyList
import cats.implicits._
import com.bones.data.{KvpCoNil, KvpCoproduct, KvpSingleValueLeft}
import com.bones.data.Value._
import com.google.protobuf.{CodedOutputStream, Timestamp}
import shapeless._

/**
  * Notes:
  * An Option[List] where the data is a some of empty list: Some(List()) becomes a None when using ProtobufSequentialInputInterpreter.
  *
  */
object ProtobufSequentialOutputInterpreter {

  type Path = Vector[String]
  type FieldNumber = Int
  type LastFieldNumber = Int
  type ComputeSize = () => Int
  type Encode =
    CodedOutputStream => Either[NonEmptyList[IOException], CodedOutputStream]
  type ComputeEncode[A] = A => (ComputeSize, Encode)
  type EncodeToProto[A] = FieldNumber => (LastFieldNumber, ComputeEncode[A])
  type EncodeHListToProto[H <: HList] =
    LastFieldNumber => (LastFieldNumber, ComputeEncode[H])
  type EncodeCoproductToProto[C<:Coproduct] =
    LastFieldNumber => (LastFieldNumber, ComputeEncode[C])

  def encodeToBytes[A](dc: BonesSchema[A]): A => Array[Byte] = dc match {
    case x: HListConvert[_, _, A] => {
      val (_, group) = kvpHList(x.from).apply(1)
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

  protected def kvpCoproduct[C<:Coproduct](co: KvpCoproduct[C]): EncodeCoproductToProto[C] = {
    co match {
      case KvpCoNil =>
        (fieldNumber: FieldNumber) => (
          fieldNumber,
          (_:CNil) =>
            (() => 0, (os: CodedOutputStream) => Right(os))
        )
      case kvp: KvpSingleValueLeft[l,r] => {
        (fieldNumber: FieldNumber) =>
          val (nextFieldLeft, leftF) = valueDefinition(kvp.kvpValue)(fieldNumber)
          val (nextFieldTail, tailF) = kvpCoproduct(kvp.kvpTail)(nextFieldLeft)
          (
            nextFieldTail,
            (input: l:+:r) =>
              {
                input match {
                  case Inl(head) => leftF(head)
                  case Inr(tail) => tailF(tail)
                }
              }
          )
      }
    }
  }

  protected def kvpHList[H <: HList, HL <: Nat](
      group: KvpHList[H, HL]): EncodeHListToProto[H] = {
    group match {
      case KvpNil =>
        (fieldNumber: FieldNumber) => (
          fieldNumber,
            (_: HNil) =>
            (() => 0, (os: CodedOutputStream) => Right(os))
          )
      case op: KvpSingleValueHead[h, t, tl, o] =>
        (fieldNumber: FieldNumber) =>
          val (nextFieldHead, headF) = valueDefinition(op.fieldDefinition.op)(fieldNumber)
          val (nextFieldTail, tailF) = kvpHList(op.tail)(nextFieldHead)
          (
            nextFieldTail,
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
          )
      case op: KvpHListHead[a, al, h, hl, t, tl] =>
        (fieldNumber: FieldNumber) =>
          implicit val split = op.split
          val (nextFieldHead, headF) = kvpHList(op.head)(fieldNumber)
          val (nextFieldTail, tailF) = kvpHList(op.tail)(nextFieldHead)
          (
            nextFieldTail,
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
          )

      case op: KvpConcreteTypeHead[a, h, n, ho, ht, nt] =>
        (fieldNumber: FieldNumber) =>
          {
            val (nextFieldHead, headF) = kvpHList(op.hListConvert.from)(fieldNumber)
            val (nextFieldTail, tailF) = kvpHList(op.tail)(nextFieldHead)
            implicit val hCons = op.isHCons
            (
              nextFieldTail,
              (input: ho) => {
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
          )
        }
    }
  }

  def valueDefinition[A](fgo: KvpValue[A]): EncodeToProto[A] = {
    fgo match {
      case op: OptionalKvpValueDefinition[a] =>
        (fieldNumber: FieldNumber) =>
          val (lastFieldNumber, fa) = valueDefinition(op.valueDefinitionOp)(fieldNumber)
          (lastFieldNumber,
            (opt: Option[a]) => {
              val optB = opt.map(fa)
              (
                () => optB.fold(0)(_._1()),
                (outputStream: CodedOutputStream) =>
                  optB.fold[Either[NonEmptyList[IOException], CodedOutputStream]](
                    Right(outputStream))(item => item._2(outputStream))
              )
            }
          )
      case ob: BooleanData =>
        (fieldNumber: FieldNumber) => (
          fieldNumber + 1,
          (bool: Boolean) =>
          (
            () => CodedOutputStream.computeBoolSize(fieldNumber, bool),
            write(_.writeBool(fieldNumber, bool))
          )
        )
      case rs: StringData =>
        (fieldNumber: FieldNumber) => (
          fieldNumber + 1,
          (str: String) =>
          (
            () => CodedOutputStream.computeStringSize(fieldNumber, str),
            write(_.writeString(fieldNumber, str))
          )
          )
      case id: ShortData =>
        (fieldNumber: FieldNumber) => (
          (fieldNumber + 1,
            (s: Short) => {
              val intValue = s.toInt
              (
                () => CodedOutputStream.computeInt32Size(fieldNumber, intValue),
                write(_.writeInt32(fieldNumber, intValue))
              )
            }
          )
        )
      case id: IntData =>
        (fieldNumber: FieldNumber) =>
          (fieldNumber + 1,
            (l: Int) =>
            (
              () => CodedOutputStream.computeInt32Size(fieldNumber, l),
              write(_.writeInt32(fieldNumber, l))
            )
          )
      case ri: LongData =>
        (fieldNumber: FieldNumber) =>
          (
            fieldNumber + 1,
            (l: Long) =>
            (
              () => CodedOutputStream.computeInt64Size(fieldNumber, l),
              write(_.writeInt64(fieldNumber, l))
            )
          )
      case uu: UuidData =>
        (fieldNumber: FieldNumber) =>
          (
            fieldNumber + 1,
            (u: UUID) =>
              (
                () => CodedOutputStream.computeStringSize(fieldNumber, u.toString),
                write(_.writeString(fieldNumber, u.toString))
              )
          )
      case dd: LocalDateTimeData =>
        (fieldNumber: FieldNumber) =>
          (
            fieldNumber + 1,
            (d: LocalDateTime) =>
            {
              val timestamp = Timestamp.newBuilder()
                .setSeconds(d.toEpochSecond(ZoneOffset.UTC))
                .setNanos(d.getNano)
                .build()
              val groupSize = timestamp.getSerializedSize

              val encodeF: Encode = (outputStream: CodedOutputStream) => {
                try {
                  outputStream.writeTag(fieldNumber, 2)
                  outputStream.writeUInt32NoTag(groupSize)
                  timestamp.writeTo(outputStream)
                  Right(outputStream)
                } catch {
                  case ex: IOException => Left(NonEmptyList.one(ex))
                }
              }
              val allSize = () => {
                groupSize + 1 + CodedOutputStream.computeUInt32SizeNoTag(groupSize)
              }
              (allSize, encodeF)
            }
          )
      case dt: LocalDateData =>
        (fieldNumber: FieldNumber) =>
          (
            fieldNumber + 1,
            (d: LocalDate) =>
            (
              () => CodedOutputStream.computeInt64Size(fieldNumber, d.toEpochDay),
              write(_.writeInt64(fieldNumber, d.toEpochDay))
            )
          )
      case fd: FloatData =>
        (fieldNumber: FieldNumber) =>
          (
            fieldNumber + 1,
            (f: Float) =>
            (
              () =>
                CodedOutputStream.computeFloatSize(fieldNumber, f),
              write(_.writeFloat(fieldNumber, f))
            )
          )
      case dd: DoubleData =>
        (fieldNumber: FieldNumber) =>
          (
            fieldNumber + 1,
            (d: Double) =>
            (
              () => CodedOutputStream.computeDoubleSize(fieldNumber, d),
              write(_.writeDouble(fieldNumber, d))
            )
          )
      case bd: BigDecimalData =>
        (fieldNumber: FieldNumber) =>
          (
            fieldNumber + 1,
            (d: BigDecimal) =>
              (
                () => CodedOutputStream.computeStringSize(fieldNumber, d.toString()),
                write(_.writeString(fieldNumber, d.toString))
              )
          )
      case ba: ByteArrayData =>
        (fieldNumber: FieldNumber) => (
          fieldNumber + 1,
          (arr: Array[Byte]) =>
          (
            () => CodedOutputStream.computeByteArraySize(fieldNumber, arr),
            write(_.writeByteArray(fieldNumber, arr))
          )
        )
      case ld: ListData[t] =>
        (fieldNumber: FieldNumber) => {
          val (lastFieldNumber, ft) = valueDefinition(ld.tDefinition)(fieldNumber)
          (
            lastFieldNumber,
            (l: List[t]) => {
              val result = l.map(item => ft(item))
              (
                () => result.map(_._1()).sum,
                write((outputStream: CodedOutputStream) =>
                  result.foreach(_._2(outputStream)))
              )
            }
          )
        }
      case ed: EitherData[a, b] =>
        val encodeToProtobufA: EncodeToProto[a] = valueDefinition(ed.definitionA)
        val encodeToProtobufB: EncodeToProto[b] = valueDefinition(ed.definitionB)


        (fieldNumber: FieldNumber) =>
          val (lastFieldNumberA, withFieldNumberA): (LastFieldNumber, a => (ComputeSize, Encode)) =
            encodeToProtobufA(fieldNumber)
          val (lastFieldNumberB, withFieldNumberB): (LastFieldNumber, b => (ComputeSize, Encode)) =
            encodeToProtobufB(lastFieldNumberA)

          (
            lastFieldNumberB,
            (output: A) =>
              output match {
                case Left(aInput) => withFieldNumberA(aInput)
                case Right(bInput) => withFieldNumberB(bInput)
              }
          )
      case esd: EnumerationData[e,a] =>
        (fieldNumber: FieldNumber) =>
          (
            fieldNumber + 1,
            (a: A) =>
              (
                () => CodedOutputStream.computeStringSize(fieldNumber, a.toString),
                write(_.writeString(fieldNumber, a.toString))
              )
          )
      case st: SumTypeData[a, b] =>
        (fieldNumber: FieldNumber) => {
          val (nextFieldNumber, enc) = valueDefinition(st.from)(fieldNumber)
          (
            nextFieldNumber,
            (out: A) =>
              {
                val a = st.fba(out)
                enc(a)
              }
          )
        }
      case kvp: KvpHListValue[h, hl] =>
        (fieldNumber: FieldNumber) =>
          val (nextFieldNumber, enc) = kvpHList(kvp.kvpHList)(1)
          (
            nextFieldNumber,
            (input: A) => enc(input.asInstanceOf[h])
          )
      case kvp: KvpCoproductValue[c] =>
        (fieldNumber: FieldNumber) =>
          val (nextFieldNumber, enc) = kvpCoproduct(kvp.kvpCoproduct)(fieldNumber)
          (
           nextFieldNumber,
            (input: A) => enc(input.asInstanceOf[c])
          )
      case x: HListConvert[h, hl, a] =>
        (fieldNumber: FieldNumber) =>
          val (_, group) = kvpHList(x.from)(1)
          (
            fieldNumber + 1,
            (a: A) =>
              {
                val hlist = x.fAtoH(a)
                val (fSize, fEncode) = group(hlist)
                val groupSize = fSize()
                val encodeF: Encode = (outputStream: CodedOutputStream) => {
                  outputStream.writeTag(fieldNumber, 2)
                  outputStream.writeUInt32NoTag(groupSize)
                  fEncode(outputStream)
                }
                val allSize = () => { groupSize + 1 + CodedOutputStream.computeUInt32SizeNoTag(groupSize) }
                (allSize, encodeF)
              }
          )
      case co: KvpCoproductConvert[c,a] =>
        (fieldNumber: FieldNumber) =>
          val (lastField,computeEncodeF) = kvpCoproduct(co.from)(fieldNumber)
          (
            lastField,
            (a: A) =>
              {
                val coproduct = co.aToC(a)
                computeEncodeF(coproduct)
              }
          )

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
