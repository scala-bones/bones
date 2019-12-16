package com.bones.protobuf

import java.io.{ByteArrayOutputStream, IOException}
import java.time.{LocalDate, LocalDateTime, ZoneOffset}
import java.util.UUID

import cats.Applicative
import cats.data.NonEmptyList
import cats.implicits._
import com.bones.data.KeyValueDefinition.CoproductDataDefinition
import com.bones.data.{KeyValueDefinition, KvpCoNil, KvpCoproduct, KvpSingleValueLeft, _}
import com.bones.syntax.NoAlgebra
import com.google.protobuf.{CodedOutputStream, Timestamp}
import shapeless._
import com.bones.protobuf.ProtobufSequentialInputInterpreter.NoAlgebraInterpreter

/**
  * Notes:
  * An Option[List] where the data is a some of empty list: Some(List()) becomes a None when using ProtobufSequentialInputInterpreter.
  *
  */
object ProtobufSequentialOutputInterpreter {

  trait CustomInterpreter[ALG[_]] {
    def encodeToProto[A](alg: ALG[A]) : EncodeToProto[A]
  }

  object NoAgebraCustomInterpreter extends CustomInterpreter[NoAlgebra] {
    def encodeToProto[A](alg: NoAlgebra[A]): EncodeToProto[A] = sys.error("Unreachable code")
  }

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

  def encodeToBytes[A](dc: BonesSchema[NoAlgebra,A]): A => Array[Byte] =
  encodeToBytesCustomAlgebra[NoAlgebra, A](dc, NoAgebraCustomInterpreter)

  def encodeToBytesCustomAlgebra[ALG[_], A](dc: BonesSchema[ALG,A], customInterpreter: CustomInterpreter[ALG]): A => Array[Byte] = dc match {
    case x: HListConvert[ALG, _, _, A] @unchecked => {
      val (_, group) = kvpHList(x.from, customInterpreter).apply(1)
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

  protected def kvpCoproduct[ALG[_], C<:Coproduct](co: KvpCoproduct[ALG, C], customInterpreter: CustomInterpreter[ALG]): EncodeCoproductToProto[C] = {
    co match {
      case nil: KvpCoNil[_] =>
        (fieldNumber: FieldNumber) => (
          fieldNumber,
          (_:CNil) =>
            (() => 0, (os: CodedOutputStream) => Right(os))
        )
      case kvp: KvpSingleValueLeft[ALG, l,r] @unchecked => {
        (fieldNumber: FieldNumber) =>
          val (nextFieldLeft, leftF) = determineValueDefinition(kvp.kvpValue, customInterpreter)(fieldNumber)
          val (nextFieldTail, tailF) = kvpCoproduct(kvp.kvpTail, customInterpreter)(nextFieldLeft)
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

  protected def kvpHList[ALG[_], H <: HList, HL <: Nat](
      group: KvpHList[ALG, H, HL], customInterpreter: CustomInterpreter[ALG]): EncodeHListToProto[H] = {
    group match {
      case nil: KvpNil[_] =>
        (fieldNumber: FieldNumber) => (
          fieldNumber,
            (_: HNil) =>
            (() => 0, (os: CodedOutputStream) => Right(os))
          )
      case op: KvpSingleValueHead[ALG, h, t, tl, o] =>
        (fieldNumber: FieldNumber) =>
          val (nextFieldHead, headF) = determineValueDefinition(op.fieldDefinition.op, customInterpreter)(fieldNumber)
          val (nextFieldTail, tailF) = kvpHList(op.tail, customInterpreter)(nextFieldHead)
          implicit val isHCons = op.isHCons
          (
            nextFieldTail,
            (input: H) =>
              {
                val hAsO = input.asInstanceOf[o]
                val headResult = headF(hAsO.head)
                val tailResult = tailF(hAsO.tail)
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
      case op: KvpHListHead[ALG, a, al, h, hl, t, tl] @unchecked =>
        (fieldNumber: FieldNumber) =>
          implicit val split = op.split
          val (nextFieldHead, headF) = kvpHList(op.head, customInterpreter)(fieldNumber)
          val (nextFieldTail, tailF) = kvpHList(op.tail, customInterpreter)(nextFieldHead)
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

      case op: KvpConcreteTypeHead[ALG, a, h, n] =>
        (fieldNumber: FieldNumber) =>
          {
            val encodeToProto: EncodeToProto[a] = op.bonesSchema match {
              case hList: HListConvert[ALG, h, n, a] @unchecked => {
                val result: EncodeHListToProto[h] = kvpHList[ALG, h, n](hList.from, customInterpreter)
                (lastFieldNumber) => {
                  val (lastField, computeEncode) : (LastFieldNumber, ComputeEncode[h]) = result(lastFieldNumber)
                  val newComputeEncode: ComputeEncode[a] = a => computeEncode.apply(hList.fAtoH(a))
                  (lastField, newComputeEncode)
                }
              }
              case co: KvpCoproductConvert[ALG, c, a] @unchecked => {
                val result: EncodeCoproductToProto[c] = kvpCoproduct(co.from, customInterpreter)
                (lastFieldNumber) => {
                  val (lastField, computeEncode) : (LastFieldNumber, ComputeEncode[c]) = result(lastFieldNumber)
                  val newComputeEncode: ComputeEncode[a] = a => computeEncode.apply(co.aToC(a))
                  (lastField, newComputeEncode)
                }
              }
            }
            val (nextFieldHead, headF) = encodeToProto(fieldNumber)
            val (nextFieldTail, tailF) = kvpHList(op.tail, customInterpreter)(nextFieldHead)
            implicit val hCons = op.isHCons
            (
              nextFieldTail,
              (input: a :: h) => {

              val headResult = headF(input.head)
              val tailResult = tailF(input.tail)
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

  def determineValueDefinition[ALG[_], A](kvp: CoproductDataDefinition[ALG, A], customInterpreter: CustomInterpreter[ALG]): EncodeToProto[A] =
    kvp match {
      case Left(kvp) => valueDefinition(kvp, customInterpreter)
      case Right(vd) => customInterpreter.encodeToProto(vd)
    }

  def valueDefinition[ALG[_], A](fgo: KvpValue[A], customInterpreter: CustomInterpreter[ALG]): EncodeToProto[A] = {
    fgo match {
      case op: OptionalKvpValueDefinition[ALG, a] @unchecked =>
        (fieldNumber: FieldNumber) =>
          val (lastFieldNumber, fa) = determineValueDefinition(op.valueDefinitionOp, customInterpreter)(fieldNumber)
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
      case ld: ListData[ALG,t] @unchecked=>
        (fieldNumber: FieldNumber) => {
          val (lastFieldNumber, ft) = determineValueDefinition(ld.tDefinition, customInterpreter)(fieldNumber)
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
      case ed: EitherData[ALG, a, b] @unchecked =>
        val encodeToProtobufA: EncodeToProto[a] = determineValueDefinition(ed.definitionA, customInterpreter)
        val encodeToProtobufB: EncodeToProto[b] = determineValueDefinition(ed.definitionB, customInterpreter)


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
      case kvp: KvpHListValue[ALG, h, hl] @unchecked =>
        (fieldNumber: FieldNumber) =>
          val (nextFieldNumber, enc) = kvpHList(kvp.kvpHList, customInterpreter)(1)
          (
            nextFieldNumber,
            (input: A) => enc(input.asInstanceOf[h])
          )
      case kvp: KvpCoproductValue[ALG,c] @unchecked =>
        (fieldNumber: FieldNumber) =>
          val (nextFieldNumber, enc) = kvpCoproduct(kvp.kvpCoproduct, customInterpreter)(fieldNumber)
          (
           nextFieldNumber,
            (input: A) => enc(input.asInstanceOf[c])
          )
      case x: HListConvert[ALG, h, hl, a] @unchecked =>
        (fieldNumber: FieldNumber) =>
          val (_, group) = kvpHList(x.from, customInterpreter)(1)
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
      case co: KvpCoproductConvert[ALG, c,a] @unchecked =>
        (fieldNumber: FieldNumber) =>
          val (lastField,computeEncodeF) = kvpCoproduct(co.from, customInterpreter)(fieldNumber)
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
