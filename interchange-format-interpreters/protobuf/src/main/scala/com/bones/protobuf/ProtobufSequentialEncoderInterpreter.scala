package com.bones.protobuf

import java.io.{ByteArrayOutputStream, IOException}
import java.time.{LocalDate, LocalDateTime, LocalTime, ZoneOffset}
import java.util.UUID

import cats.Applicative
import cats.data.NonEmptyList
import cats.implicits._
import com.bones.data.KeyValueDefinition.CoproductDataDefinition
import com.bones.data.values.CNilF
import com.bones.data.{KvpCoNil, KvpCoproduct, KvpSingleValueLeft, _}
import com.google.protobuf.{CodedOutputStream, Timestamp}
import shapeless._

object ProtobufSequentialEncoderInterpreter {

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
  type EncodeCoproductToProto[C <: Coproduct] =
    LastFieldNumber => (LastFieldNumber, ComputeEncode[C])

  /** Run code against the CodedOutputStream and watch for errors */
  def write(f: CodedOutputStream => Unit): Encode =
    (codedOutputStream: CodedOutputStream) =>
      try {
        f(codedOutputStream)
        Right(codedOutputStream)
      } catch {
        case ex: IOException => Left(NonEmptyList.one(ex))
    }

  def determineValueDefinition[ALG[_], A](
                                           kvp: CoproductDataDefinition[ALG, A],
                                           valueDefinition: (ConcreteValue[ALG, A], ProtobufEncoderValue[ALG]) => EncodeToProto[A],
                                           customInterpreter: ProtobufEncoderValue[ALG]
  ): EncodeToProto[A] =
    kvp match {
      case Left(kvp) => valueDefinition(kvp, customInterpreter)
      case Right(vd) => customInterpreter.encodeToProto(vd)
    }

  def optionalKvpValueDefinition[ALG[_], B](
                                             op: OptionalValue[ALG, B],
                                             valueDefinition: (ConcreteValue[ALG, B], ProtobufEncoderValue[ALG]) => EncodeToProto[B],
                                             customInterpreter: ProtobufEncoderValue[ALG]
  ): EncodeToProto[Option[B]] = { (fieldNumber: FieldNumber) =>
    val (lastFieldNumber, fa) =
      determineValueDefinition(op.valueDefinitionOp, valueDefinition, customInterpreter)(
        fieldNumber)
    (lastFieldNumber, (opt: Option[B]) => {
      val optB = opt.map(fa)
      (
        () => optB.fold(0)(_._1()),
        (outputStream: CodedOutputStream) =>
          optB.fold[Either[NonEmptyList[IOException], CodedOutputStream]](Right(outputStream))(
            item => item._2(outputStream))
      )
    })
  }

  val booleanData: EncodeToProto[Boolean] =
    (fieldNumber: FieldNumber) =>
      (
        fieldNumber + 1,
        (bool: Boolean) =>
          (
            () => CodedOutputStream.computeBoolSize(fieldNumber, bool),
            write(_.writeBool(fieldNumber, bool))
        )
    )

  val stringData = stringDataFromMap[String](identity)

  def stringDataFromMap[A](f: A => String): EncodeToProto[A] =
    (fieldNumber: FieldNumber) =>
      (
        fieldNumber + 1,
        (a: A) => {
          val mapped = f(a)
          (
            () => CodedOutputStream.computeStringSize(fieldNumber, mapped),
            write(_.writeString(fieldNumber, mapped))
          )
        }
    )

  /** Can't go shorter than an Int */
  val shortData = intDataFromMap[Short](_.toInt)

  val intData = intDataFromMap[Int](identity)
  def intDataFromMap[A](f: A => Int): EncodeToProto[A] =
    (fieldNumber: FieldNumber) =>
      (fieldNumber + 1, (l: A) => {
        val mapped = f(l)
        (
          () => CodedOutputStream.computeInt32Size(fieldNumber, mapped),
          write(_.writeInt32(fieldNumber, mapped))
        )
      })

  val longData = longDataFromMap[Long](identity)
  def longDataFromMap[A](f: A => Long): EncodeToProto[A] =
    (fieldNumber: FieldNumber) =>
      (
        fieldNumber + 1,
        (l: A) => {
          val mapped = f(l)
          (
            () => CodedOutputStream.computeInt64Size(fieldNumber, mapped),
            write(_.writeInt64(fieldNumber, mapped))
          )
        }
    )

  def localDateTimeToSecondsNanos(zoneOffset: ZoneOffset): LocalDateTime => (Long, Int) =
    localDateTime => (localDateTime.toEpochSecond(zoneOffset), localDateTime.getNano)

  def timestampFromMap[A](f: A => (Long, Int)): EncodeToProto[A] = { (fieldNumber: FieldNumber) =>
    (
      fieldNumber + 1,
      (d: A) => {
        val (seconds, nanos) = f(d)
        val timestamp = Timestamp
          .newBuilder()
          .setSeconds(seconds)
          .setNanos(nanos)
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
  }

  val floatData: EncodeToProto[Float] =
    (fieldNumber: FieldNumber) =>
      (
        fieldNumber + 1,
        (f: Float) =>
          (
            () => CodedOutputStream.computeFloatSize(fieldNumber, f),
            write(_.writeFloat(fieldNumber, f))
        )
    )

  val doubleData: EncodeToProto[Double] =
    (fieldNumber: FieldNumber) =>
      (
        fieldNumber + 1,
        (d: Double) =>
          (
            () => CodedOutputStream.computeDoubleSize(fieldNumber, d),
            write(_.writeDouble(fieldNumber, d))
        )
    )
  val bigDecimalData: EncodeToProto[BigDecimal] = stringDataFromMap[BigDecimal](_.toString)

  val byteArrayData: EncodeToProto[Array[Byte]] =
    (fieldNumber: FieldNumber) =>
      (
        fieldNumber + 1,
        (arr: Array[Byte]) =>
          (
            () => CodedOutputStream.computeByteArraySize(fieldNumber, arr),
            write(_.writeByteArray(fieldNumber, arr))
        )
    )

  def enumerationData[A]: EncodeToProto[A] = stringDataFromMap[A](_.toString)

}

/**
  * Notes:
  * An Option[List] where the data is a some of empty list: Some(List()) becomes a None when using ProtobufSequentialValidatorInterpreter.
  *
  */
trait ProtobufSequentialEncoderInterpreter {

  import ProtobufSequentialEncoderInterpreter._

  def generateProtobufEncoder[ALG[_], A](
                                          dc: ConcreteValue[ALG, A],
                                          customInterpreter: ProtobufEncoderValue[ALG]): A => Array[Byte] = dc match {
    case x: Switch[ALG, _, _, A] @unchecked => {
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
    case _ => ??? // TODO

  }

  protected def kvpCoproduct[ALG[_], C <: Coproduct](
    co: KvpCoproduct[ALG, C],
    customInterpreter: ProtobufEncoderValue[ALG]
  ): EncodeCoproductToProto[C] = {
    co match {
      case nil: KvpCoNil[_] =>
        (fieldNumber: FieldNumber) =>
          (
            fieldNumber,
            (_: CNil) => (() => 0, (os: CodedOutputStream) => Right(os))
          )
      case kvp: KvpSingleValueLeft[ALG, l, r] @unchecked => { (fieldNumber: FieldNumber) =>
        val (nextFieldLeft, leftF) = determineValueDefinition[ALG, l](
          kvp.kvpValue,
          valueDefinition[ALG, l],
          customInterpreter)(fieldNumber)
        val (nextFieldTail, tailF) = kvpCoproduct(kvp.kvpTail, customInterpreter)(nextFieldLeft)
        (
          nextFieldTail,
          (input: l :+: r) => {
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
                                                         group: KvpCollection[ALG, H, HL],
                                                         customInterpreter: ProtobufEncoderValue[ALG]): EncodeHListToProto[H] = {
    group match {
      case nil: KvpNil[_] =>
        (fieldNumber: FieldNumber) =>
          (
            fieldNumber,
            (_: HNil) => (() => 0, (os: CodedOutputStream) => Right(os))
          )
      case op: KvpSingleValueHead[ALG, h, t, tl, o] =>
        (fieldNumber: FieldNumber) =>
          val (nextFieldHead, headF) = determineValueDefinition[ALG, h](
            op.fieldDefinition.dataDefinition,
            valueDefinition[ALG, h],
            customInterpreter)(fieldNumber)
          val (nextFieldTail, tailF) = kvpHList(op.tail, customInterpreter)(nextFieldHead)
          implicit val isHCons = op.isHCons
          (
            nextFieldTail,
            (input: H) => {
              val hAsO = input.asInstanceOf[o]
              val headResult = headF(hAsO.head)
              val tailResult = tailF(hAsO.tail)
              val fCompute: ComputeSize = () => headResult._1() + tailResult._1()
              val fEncode: Encode = (outputStream: CodedOutputStream) => {
                Applicative[Either[NonEmptyList[IOException], ?]]
                  .map2(headResult._2(outputStream), tailResult._2(outputStream))(
                    (l1: CodedOutputStream, l2: CodedOutputStream) => {
                      l2
                    })
              }
              (fCompute, fEncode)
            }
          )
      case op: KvpCollectionHead[ALG, a, al, h, hl, t, tl] @unchecked =>
        (fieldNumber: FieldNumber) =>
          implicit val split = op.split
          val (nextFieldHead, headF) = kvpHList(op.head, customInterpreter)(fieldNumber)
          val (nextFieldTail, tailF) = kvpHList(op.tail, customInterpreter)(nextFieldHead)
          (
            nextFieldTail,
            (input: H) => {
              val (head, tail) = split(input)
              val headResult = headF(head)
              val tailResult = tailF(tail)
              val fCompute: ComputeSize = () => headResult._1() + tailResult._1()
              val fEncode = (outputStream: CodedOutputStream) => {
                Applicative[Either[NonEmptyList[IOException], ?]]
                  .map2(headResult._2(outputStream), tailResult._2(outputStream))(
                    (l1: CodedOutputStream, l2: CodedOutputStream) => {
                      l2
                    })
              }
              (fCompute, fEncode)
            }
          )

      case op: KvpConcreteValueHead[ALG, a, h, n] =>
        (fieldNumber: FieldNumber) =>
          {
            val encodeToProto: EncodeToProto[a] = op.collection match {
              case hList: Switch[ALG, h, n, a] @unchecked => {
                val result: EncodeHListToProto[h] =
                  kvpHList[ALG, h, n](hList.from, customInterpreter)
                (lastFieldNumber) =>
                  {
                    val (lastField, computeEncode): (LastFieldNumber, ComputeEncode[h]) = result(
                      lastFieldNumber)
                    val newComputeEncode: ComputeEncode[a] = a =>
                      computeEncode.apply(hList.fAtoH(a))
                    (lastField, newComputeEncode)
                  }
              }
              case co: CoproductSwitch[ALG, c, a] @unchecked => {
                val result: EncodeCoproductToProto[c] = kvpCoproduct(co.from, customInterpreter)
                (lastFieldNumber) =>
                  {
                    val (lastField, computeEncode): (LastFieldNumber, ComputeEncode[c]) = result(
                      lastFieldNumber)
                    val newComputeEncode: ComputeEncode[a] = a => computeEncode.apply(co.aToC(a))
                    (lastField, newComputeEncode)
                  }
              }
              case _ => ??? // TODO
            }
            val (nextFieldHead, headF) = encodeToProto(fieldNumber)
            val (nextFieldTail, tailF) = kvpHList(op.tail, customInterpreter)(nextFieldHead)
            implicit val hCons = op.isHCons
            (
              nextFieldTail,
              (input: a :: h) => {

                val headResult = headF(input.head)
                val tailResult = tailF(input.tail)
                val fCompute: ComputeSize = () => headResult._1() + tailResult._1()
                val fEncode = (outputStream: CodedOutputStream) => {
                  Applicative[Either[NonEmptyList[IOException], ?]]
                    .map2(headResult._2(outputStream), tailResult._2(outputStream))(
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

  def valueDefinition[ALG[_], A](
                                  fgo: ConcreteValue[ALG, A],
                                  customInterpreter: ProtobufEncoderValue[ALG]): EncodeToProto[A] = {
    fgo match {
      case op: OptionalValue[ALG, a] @unchecked =>
        optionalKvpValueDefinition[ALG, a](op, valueDefinition, customInterpreter)

      case ld: ListData[ALG, t] @unchecked =>
        (fieldNumber: FieldNumber) =>
          {
            val (lastFieldNumber, ft) = determineValueDefinition[ALG, t](
              ld.tDefinition,
              valueDefinition[ALG, t],
              customInterpreter)(fieldNumber)
            (
              lastFieldNumber,
              (l: List[t]) => {
                val result = l.map(item => ft(item))
                (
                  () => result.map(_._1()).sum,
                  write((outputStream: CodedOutputStream) => result.foreach(_._2(outputStream)))
                )
              }
            )
          }
      case ed: EitherData[ALG, a, b] @unchecked =>
        val encodeToProtobufA: EncodeToProto[a] =
          determineValueDefinition(ed.definitionA, valueDefinition[ALG, a], customInterpreter)
        val encodeToProtobufB: EncodeToProto[b] =
          determineValueDefinition(ed.definitionB, valueDefinition[ALG, b], customInterpreter)

        (fieldNumber: FieldNumber) =>
          val (lastFieldNumberA, withFieldNumberA): (LastFieldNumber, a => (ComputeSize, Encode)) =
            encodeToProtobufA(fieldNumber)
          val (lastFieldNumberB, withFieldNumberB): (LastFieldNumber, b => (ComputeSize, Encode)) =
            encodeToProtobufB(lastFieldNumberA)

          (
            lastFieldNumberB,
            (output: A) =>
              output match {
                case Left(aInput)  => withFieldNumberA(aInput)
                case Right(bInput) => withFieldNumberB(bInput)
            }
          )
      case kvp: KvpHListValue[ALG, h, hl] @unchecked =>
        (fieldNumber: FieldNumber) =>
          val (nextFieldNumber, enc) = kvpHList(kvp.kvpHList, customInterpreter)(1)
          (
            nextFieldNumber,
            (input: A) => enc(input.asInstanceOf[h])
          )
      case kvp: CoproductCollection[ALG, c] @unchecked =>
        (fieldNumber: FieldNumber) =>
          val (nextFieldNumber, enc) =
            kvpCoproduct(kvp.kvpCoproduct, customInterpreter)(fieldNumber)
          (
            nextFieldNumber,
            (input: A) => enc(input.asInstanceOf[c])
          )
      case x: Switch[ALG, h, hl, a] @unchecked =>
        (fieldNumber: FieldNumber) =>
          val (_, group) = kvpHList(x.from, customInterpreter)(1)
          (
            fieldNumber + 1,
            (a: A) => {
              val hlist = x.fAtoH(a)
              val (fSize, fEncode) = group(hlist)
              val groupSize = fSize()
              val encodeF: Encode = (outputStream: CodedOutputStream) => {
                outputStream.writeTag(fieldNumber, 2)
                outputStream.writeUInt32NoTag(groupSize)
                fEncode(outputStream)
              }
              val allSize = () => {
                groupSize + 1 + CodedOutputStream.computeUInt32SizeNoTag(groupSize)
              }
              (allSize, encodeF)
            }
          )
      case co: CoproductSwitch[ALG, c, a] @unchecked =>
        (fieldNumber: FieldNumber) =>
          val (lastField, computeEncodeF) = kvpCoproduct(co.from, customInterpreter)(fieldNumber)
          (
            lastField,
            (a: A) => {
              val coproduct = co.aToC(a)
              computeEncodeF(coproduct)
            }
          )
    }
  }

}
