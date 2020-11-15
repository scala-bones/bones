package com.bones.protobuf

import java.io.{ByteArrayOutputStream, IOException}
import java.time.{LocalDateTime, ZoneOffset}

import cats.Applicative
import cats.data.NonEmptyList
import com.bones.data.KeyDefinition.CoproductDataDefinition
import com.bones.data.{KvpCoNil, KvpCoproduct, KvpCoproductCollectionHead, _}
import com.google.protobuf.{CodedOutputStream, Timestamp}
import shapeless._

object ProtobufSequentialEncoderInterpreter {

  /** Run code against the CodedOutputStream and watch for errors */
  def write(f: CodedOutputStream => Unit): Encode =
    (codedOutputStream: CodedOutputStream) =>
      try {
        f(codedOutputStream)
        Right(codedOutputStream)
      } catch {
        case ex: IOException => Left(NonEmptyList.one(ex))
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
trait ProtobufSequentialEncoderInterpreter[ALG[_]] {

  def customInterpreter: ProtobufEncoderValue[ALG]

  import ProtobufSequentialEncoderInterpreter._

  def determineValueDefinition[A](
    kvp: CoproductDataDefinition[String, ALG, A]
  ): EncodeToProto[A] =
    kvp match {
      case Left(kvp) => valueDefinition(kvp)
      case Right(vd) => customInterpreter.encodeToProto(vd)
    }

  def optionalKvpValueDefinition[B](op: OptionalValue[String, ALG, B]): EncodeToProto[Option[B]] = {
    (fieldNumber: FieldNumber) =>
      val (lastFieldNumber, computeEncode) =
        determineValueDefinition(op.valueDefinitionOp)(fieldNumber)
      (lastFieldNumber, (opt: Option[B]) => {
        val optB = opt.map(computeEncode)
        (
          () => optB.fold(0)(_._1()),
          (outputStream: CodedOutputStream) =>
            optB.fold[Either[NonEmptyList[IOException], CodedOutputStream]](Right(outputStream))(
              item => item._2(outputStream))
        )
      })
  }

  def generateProtobufEncoder[A](dc: KvpCollection[String, ALG, A]): A => Array[Byte] = {
    val (_, group) = fromKvpCollection(dc).apply(1)
    (a: A) =>
      {
        val (_, fEncode) = group(a)
        val os = new ByteArrayOutputStream()
        val cos: CodedOutputStream = CodedOutputStream.newInstance(os)
        fEncode(cos)
        cos.flush()
        os.flush()
        os.close()
        os.toByteArray
      }
  }

  protected def kvpCoproduct[C <: Coproduct](
    co: KvpCoproduct[String, ALG, C]
  ): EncodeToProto[C] = {
    co match {
      case nil: KvpCoNil[String, _] @unchecked =>
        (fieldNumber: FieldNumber) =>
          (
            fieldNumber,
            (_: C) => (() => 0, (os: CodedOutputStream) => Right(os))
          )
      case kvp: KvpCoproductCollectionHead[String, ALG, l, r, C] @unchecked => {
        (fieldNumber: FieldNumber) =>
          val (nextFieldLeft, leftComputeEncode) =
            fromKvpCollection[l](kvp.kvpCollection)(fieldNumber)
          val (nextFieldTail, tailComputeEncode) = kvpCoproduct(kvp.kvpTail)(nextFieldLeft)
          (
            nextFieldTail,
            (input: C) => {
              input match {
                case Inl(head) => leftComputeEncode(head.asInstanceOf[l])
                case Inr(tail) => tailComputeEncode(tail.asInstanceOf[r])
              }
            }
          )
      }
    }
  }

  def fromKvpCollection[A](group: KvpCollection[String, ALG, A]): EncodeToProto[A] = {
    group match {
      case nil: KvpNil[String, _] @unchecked =>
        (fieldNumber: FieldNumber) =>
          (
            fieldNumber,
            (_: A) => (() => 0, (os: CodedOutputStream) => Right(os))
          )
      case op: KvpSingleValueHead[String, ALG, h, t, tl, o] =>
        (fieldNumber: FieldNumber) =>
          val (nextFieldHead, headF) = op.head match {
            case Left(keyDef)         => determineValueDefinition(keyDef.dataDefinition)(fieldNumber)
            case Right(kvpCollection) => fromKvpCollection(kvpCollection)(fieldNumber)
          }
          val (nextFieldTail, tailF) = fromKvpCollection(op.tail)(nextFieldHead)

          implicit val isHCons = op.isHCons
          (
            nextFieldTail,
            (input: A) => {
              val headResult = headF(isHCons.head(input.asInstanceOf[o]))
              val tailResult = tailF(isHCons.tail(input.asInstanceOf[o]))
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
      case op: KvpHListCollectionHead[String, ALG, ho, hon, h, hl, t, tl] @unchecked =>
        (fieldNumber: FieldNumber) =>
          implicit val split = op.split
          val (nextFieldHead, headF) = fromKvpCollection(op.head)(fieldNumber)
          val (nextFieldTail, tailF) = fromKvpCollection(op.tail)(nextFieldHead)
          (
            nextFieldTail,
            (input: A) => {
              val (head, tail) = split(input.asInstanceOf[ho])
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
      case op: KvpWrappedCoproduct[String, ALG, A, c] @unchecked =>
        mapEncodeToProto(op.fAtoC, fromKvpCollection(op.wrappedEncoding))
      case op: KvpWrappedHList[String, ALG, A, xs, xsl] @unchecked =>
        mapEncodeToProto(op.fAtoH, fromKvpCollection(op.wrappedEncoding))
      case co: KvpCoproduct[String, ALG, c] @unchecked =>
        kvpCoproduct[c](co).asInstanceOf[EncodeToProto[A]]
    }
  }

  private def wrappedChild[A, B](
    f: A => B,
    child: KvpCollection[String, ALG, B]): EncodeToProto[A] =
    fieldNumber => {
      (fieldNumber + 1, (a: A) => {
        val b = f(a)
        val (_, computeEncode) = fromKvpCollection(child).apply(1)
        val (fSize, fEncode) = computeEncode.apply(b)
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
      })
    }

  private def subObject[A](child: KvpCollectionValue[String, ALG, A]): EncodeToProto[A] =
    fieldNumber => {
      (fieldNumber + 1, (a: A) => {
        val (_, computeEncode) = fromKvpCollection(child.kvpCollection).apply(1)
        val (fSize, fEncode) = computeEncode.apply(a)
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
      })
    }

  def valueDefinition[A](fgo: HigherOrderValue[String, ALG, A]): EncodeToProto[A] = {
    fgo match {
      case op: OptionalValue[String, ALG, a] @unchecked =>
        optionalKvpValueDefinition[a](op).asInstanceOf[EncodeToProto[A]]

      case ld: ListData[String, ALG, t] @unchecked =>
        (fieldNumber: FieldNumber) =>
          {
            val (lastFieldNumber, ft) = determineValueDefinition[t](ld.tDefinition)(fieldNumber)
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
      case ed: EitherData[String, ALG, a, b] @unchecked =>
        val encodeToProtobufA: EncodeToProto[a] =
          determineValueDefinition(ed.definitionA)
        val encodeToProtobufB: EncodeToProto[b] =
          determineValueDefinition(ed.definitionB)

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
      case kvp: KvpCollectionValue[String, ALG, A] @unchecked =>
        fromKvpCollection(kvp.kvpCollection)

    }
  }

}
