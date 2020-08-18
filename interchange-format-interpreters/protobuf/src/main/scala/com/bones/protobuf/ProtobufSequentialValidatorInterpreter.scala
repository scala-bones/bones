package com.bones.protobuf

import java.io.{ByteArrayInputStream, IOException}
import java.time.{LocalDateTime, ZoneOffset}

import cats.data.NonEmptyList
import cats.implicits._
import com.bones.Util
import com.bones.data.Error._
import com.bones.data.values.CNilF
import com.bones.data.{KvpCoNil, KvpCoproduct, KvpCoproductCollectionHead, _}
import com.bones.validation.ValidationDefinition.ValidationOp
import com.bones.validation.{ValidationUtil => vu}
import com.google.protobuf.{CodedInputStream, InvalidProtocolBufferException, Timestamp}
import shapeless.{:+:, Coproduct, HList, HNil, Inl, Inr, Nat}

import scala.annotation.tailrec

object ProtobufSequentialValidatorInterpreter {

  /** Path to the value -- list of keys */
  type Path = List[String]
  type LastFieldNumber = Int
  type Tag = Int
  type CanReadTag = Boolean

  /**
    * Given the last field number from loading the previous value, as well as the path to the value
    * the interpreter will return the list of nested tags and the LastFieldNumber of the next values
    * to be read in.  Also return a function
    *
    * @tparam A
    */
  type ExtractFromProto[A] =
    (LastFieldNumber, Path) => (
      List[Tag],
      LastFieldNumber,
      (CanReadTag, CodedInputStream) => (CanReadTag, Either[NonEmptyList[ExtractionError], A])
    )
  type ExtractHListFromProto[H <: HList] =
    (LastFieldNumber, Path) => (
      LastFieldNumber,
      (CanReadTag, CodedInputStream) => (CanReadTag, Either[NonEmptyList[ExtractionError], H]))

  type ExtractProductFromProto[C <: Coproduct] =
    (LastFieldNumber, Path) => (
      List[Tag],
      LastFieldNumber,
      (CanReadTag, CodedInputStream) => (CanReadTag, Either[NonEmptyList[ExtractionError], C]))

  /** Determine if we use the core algebra or the custom algebra and then pass control to the appropriate interpreter */
  def determineValueDefinition[ALG[_], A](
    definition: Either[PrimitiveWrapperValue[ALG, A], ALG[A]],
    valueDefinition: (
      PrimitiveWrapperValue[ALG, A],
      ProtobufValidatorValue[ALG]) => ExtractFromProto[A],
    customInterpreter: ProtobufValidatorValue[ALG]
  ): ExtractFromProto[A] =
    definition match {
      case Left(kvp)  => valueDefinition(kvp, customInterpreter)
      case Right(alg) => customInterpreter.extractFromProto(alg)
    }

  private def convert[A](
    in: CodedInputStream,
    clazz: Class[A],
    path: List[String]
  )(f: CodedInputStream => A): Either[NonEmptyList[CanNotConvert[CodedInputStream, A]], A] =
    try {
      Right(f(in))
    } catch {
      case e: IOException =>
        Left(NonEmptyList.one(CanNotConvert(path, in, clazz, Some(e))))
    }

  // For FieldNumber definitions
  // see https://developers.google.com/protocol-buffers/docs/encoding
  private val VARINT = 0 //	Varint	int32, int64, uint32, uint64, sint32, sint64, bool, enum
  private val BIT64 = 1 // 64-bit	fixed64, sfixed64, double
  private val LENGTH_DELIMITED = 2 // Length-delimited	string, bytes, embedded messages, packed repeated fields
  private val BIT32 = 5 // 32-bit	fixed32, sfixed32, float

  def identitySuccess[A]: (A, Path) => Either[NonEmptyList[ExtractionError], A] = (a, _) => Right(a)

  def optionalKvpValueDefinition[ALG[_], B](
    op: OptionalValue[ALG, B],
    valueDefinition: (
      PrimitiveWrapperValue[ALG, B],
      ProtobufValidatorValue[ALG]) => ExtractFromProto[B],
    customInterpreter: ProtobufValidatorValue[ALG]
  ): ExtractFromProto[Option[B]] = {
    val vd = determineValueDefinition(op.valueDefinitionOp, valueDefinition, customInterpreter)
    (fieldNumber: LastFieldNumber, path: Path) =>
      {
        val (tags, childFieldNumber, fa) = vd(fieldNumber, path)
        (tags, childFieldNumber, (canReadInput, in) => {
          if (tags.contains(in.getLastTag)) {
            val (canRead, result) = fa(canReadInput, in)
            (canRead, result.map(Some(_)))
          } else {
            (canReadInput, Right(None))
          }
        }: (CanReadTag, Either[NonEmptyList[ExtractionError], Option[B]]))
      }
  }

  def listData[ALG[_], B](
    ld: ListData[ALG, B],
    valueDefinition: (
      PrimitiveWrapperValue[ALG, B],
      ProtobufValidatorValue[ALG]) => ExtractFromProto[B],
    customInterpreter: ProtobufValidatorValue[ALG],
    validations: List[ValidationOp[List[B]]]
  ): ExtractFromProto[List[B]] = {
    val child = determineValueDefinition[ALG, B](ld.tDefinition, valueDefinition, customInterpreter)

    @tailrec
    def loop[C](
      tags: List[Int],
      canReadListTag: Boolean,
      codedInputStream: CodedInputStream,
      path: List[String],
      accumulated: List[Either[NonEmptyList[ExtractionError], C]],
      f: (CanReadTag, CodedInputStream) => (CanReadTag, Either[NonEmptyList[ExtractionError], C])
    ): List[Either[NonEmptyList[ExtractionError], C]] = {
      if (canReadListTag) {
        codedInputStream.readTag()
      }
      val lastTag = codedInputStream.getLastTag
      if (!tags.contains(lastTag)) {
        accumulated
      } else {
        val (canReadTag, nextValue) = f(false, codedInputStream)
        val result = accumulated :+ nextValue
        loop(tags, canReadTag, codedInputStream, path, result, f)
      }
    }

    (fieldNumber: LastFieldNumber, path: Path) =>
      {
        val (tags, lastFieldNumber, f) = child(fieldNumber, path)
        (tags, lastFieldNumber, (canReadTag, in) => {
          val loopResult = loop(tags, canReadTag, in, path, List.empty, f).sequence
            .flatMap(i => vu.validate(validations)(i, path))
          (false, loopResult) //we read in tag to see the last value
        })
      }
  }

  def eitherData[ALG[_], B, C](
    ed: EitherData[ALG, B, C],
    valueDefinitionB: (
      PrimitiveWrapperValue[ALG, B],
      ProtobufValidatorValue[ALG]) => ExtractFromProto[B],
    valueDefinitionC: (
      PrimitiveWrapperValue[ALG, C],
      ProtobufValidatorValue[ALG]) => ExtractFromProto[C],
    customInterpreter: ProtobufValidatorValue[ALG],
  ): ExtractFromProto[Either[B, C]] = {
    val extractA: ExtractFromProto[B] =
      determineValueDefinition[ALG, B](ed.definitionA, valueDefinitionB, customInterpreter)
    val extractB: ExtractFromProto[C] =
      determineValueDefinition[ALG, C](ed.definitionB, valueDefinitionC, customInterpreter)
    (lastFieldNumber, path) =>
      {
        val (tagsA, fieldNumberA, cisFA) = extractA(lastFieldNumber, path)
        val (tagsB, fieldNumberB, cisFB) = extractB(fieldNumberA, path)
        (tagsA ::: tagsB, fieldNumberB, (canReadTag: CanReadTag, in: CodedInputStream) => {
          if (tagsA.contains(in.getLastTag)) {
            val (canReadResult, result) = cisFA(canReadTag, in)
            (canReadResult, result.map(Left(_)))
          } else if (tagsB.contains(in.getLastTag)) {
            val (canReadResult, result) = cisFB(canReadTag, in)
            (canReadResult, result.map(Right(_)))
          } else {
            (canReadTag, Left(NonEmptyList.one(RequiredValue(path, Left(ed)))))
          }
        }: (CanReadTag, Either[NonEmptyList[ExtractionError], Either[B, C]]))

      }
  }

  def kvpCoproductValueData[ALG[_], A, C <: Coproduct](
    kvp: CoproductCollection[ALG, C],
    kvpCoproduct: (
      KvpCoproduct[ALG, C],
      PrimitiveWrapperValue[ALG, C],
      ProtobufValidatorValue[ALG]) => ExtractProductFromProto[C],
    customInterpreter: ProtobufValidatorValue[ALG]
  ): ExtractFromProto[A] = {
    val group = kvpCoproduct(kvp.kvpCoproduct, kvp, customInterpreter)
    (last: LastFieldNumber, path: Path) =>
      {
        val (tags, lastFieldNumber, coproductF) = group(last, path)
        val f = (canReadInput: CanReadTag, in: CodedInputStream) => {
          val (canRead, result) = coproductF(canReadInput, in)
          (canRead, result.map(_.asInstanceOf[A]))
        }
        (tags, lastFieldNumber, f)
      }
  }

  def kvpHListValue[ALG[_], A, H <: HList, HL <: Nat](
    kvp: KvpCollectionValue[ALG, H, HL],
    kvpHList: (
      KvpHListCollection[ALG, H, HL],
      ProtobufValidatorValue[ALG]) => ExtractHListFromProto[H],
    customInterpreter: ProtobufValidatorValue[ALG]
  ): ExtractFromProto[A] = {
    val groupExtract = kvpHList(kvp.kvpCollection, customInterpreter)
    (last: LastFieldNumber, path: Path) =>
      {
        val tag = (last + 1) << 3 | LENGTH_DELIMITED
        val (lastFieldNumber, f) = groupExtract.apply(0, path)
        (List(tag), lastFieldNumber, (canReadInput: CanReadTag, in: CodedInputStream) => {
          val length = in.readRawVarint32()
          val oldLimit = in.pushLimit(length)
          val result = f(canReadInput, in)
          try {
            in.checkLastTagWas(0)
            in.popLimit(oldLimit)
            (true, result.asInstanceOf[Either[NonEmptyList[ExtractionError], A]])
          } catch {
            case ex: InvalidProtocolBufferException => {
              in.getLastTag
              (
                true,
                Left(
                  NonEmptyList.one(WrongTypeError(path, classOf[HList], classOf[Any], Some(ex)))))
            }
          }
        }: (CanReadTag, Either[NonEmptyList[ExtractionError], A]))
      }
  }

  def hListConvert[ALG[_], A, H <: HList, HL <: Nat](
    kvp: SwitchEncoding[ALG, H, HL, A],
    kvpHList: (
      KvpHListCollection[ALG, H, HL],
      ProtobufValidatorValue[ALG]) => ExtractHListFromProto[H],
    customInterpreter: ProtobufValidatorValue[ALG]
  ): ExtractFromProto[A] = {
    val groupExtract = kvpHList(kvp.from, customInterpreter)
    (last: LastFieldNumber, path: Path) =>
      {
        val thisTag = last << 3 | LENGTH_DELIMITED
        val (lastFieldNumber, fIn) = groupExtract(1, path)
        (List(thisTag), last + 1, (canReadTag: CanReadTag, in: CodedInputStream) => {
          val length = in.readRawVarint32()
          val oldLimit = in.pushLimit(length)
          val (_, result) = fIn(true, in)
          try {
            in.readTag() //should be 0
            in.checkLastTagWas(0)
            in.popLimit(oldLimit)
            (
              true,
              result
                .map(kvp.fHtoA(_))
                .asInstanceOf[Either[NonEmptyList[ExtractionError], A]]
                .flatMap(i => vu.validate(kvp.validations)(i, path)))
          } catch {
            case ex: InvalidProtocolBufferException => {
              in.getLastTag
              (
                canReadTag,
                Left(
                  NonEmptyList.one(WrongTypeError(path, classOf[HList], classOf[Any], Some(ex)))))
            }
          }
        })
      }
  }

  def kvpCoproductConvert[ALG[_], C <: Coproduct, A](
    co: CoproductSwitch[ALG, C, A],
    kvpCoproduct: (
      KvpCoproduct[ALG, C],
      PrimitiveWrapperValue[ALG, A],
      ProtobufValidatorValue[ALG]) => ExtractProductFromProto[C],
    customInterpreter: ProtobufValidatorValue[ALG]
  ): ExtractFromProto[A] = {
    val coExtract = kvpCoproduct(co.from, co, customInterpreter)
    (last: LastFieldNumber, path: Path) =>
      {
        val (tags, lastFieldNumber, f) = coExtract(last, path)
        val newF = (canReadTag: CanReadTag, in: CodedInputStream) => {
          val (newCanReadTag, either) = f(canReadTag, in)
          (
            newCanReadTag,
            either
              .map(coproduct => co.cToA(coproduct))
              .flatMap(i => vu.validate(co.validations)(i, path)))
        }
        (tags, lastFieldNumber, newF)
      }
  }

  /** Returns a function reads boolean data from the codedInputStream */
  def booleanData[ALG[_], A](
    coproductDataDefinition: ALG[A],
    validations: List[ValidationOp[Boolean]]): ExtractFromProto[Boolean] =
    (fieldNumber: LastFieldNumber, path: Path) => {
      val thisTag = fieldNumber << 3 | VARINT
      (List(thisTag), fieldNumber + 1, (canReadTag: CanReadTag, in: CodedInputStream) => {
        if (in.getLastTag == thisTag) {
          (
            true,
            convert(in, classOf[Boolean], path)(_.readBool()).flatMap(bool =>
              vu.validate(validations)(bool, path)))
        } else {
          (canReadTag, Left(NonEmptyList.one(RequiredValue(path, Right(coproductDataDefinition)))))
        }
      }: (CanReadTag, Either[NonEmptyList[ExtractionError], Boolean]))
    }

  /** Returns a function which reads String data as UTF from the CodedInputStream */
  def stringData[ALG[_]](
    alg: ALG[String],
    validations: List[ValidationOp[String]]): ExtractFromProto[String] =
    stringDataWithFlatMap(alg, identitySuccess, validations)

  def stringDataWithFlatMap[ALG[_], A](
    alg: ALG[A],
    f: (String, Path) => Either[NonEmptyList[ExtractionError], A],
    validations: List[ValidationOp[A]]): ExtractFromProto[A] =
    (fieldNumber: LastFieldNumber, path: Path) => {
      val thisTag = fieldNumber << 3 | LENGTH_DELIMITED
      (List(thisTag), fieldNumber + 1, (canReadTag, in) => {
        val lastTag = in.getLastTag
        if (lastTag == thisTag) {
          val utfStringResult =
            convert(in, classOf[String], path)(cis => cis.readStringRequireUtf8())
          val functionApplied = utfStringResult
            .flatMap(str => f(str, path))
            .flatMap(bool => vu.validate(validations)(bool, path))
          (true, functionApplied)
        } else {
          (canReadTag, Left(NonEmptyList.one(RequiredValue(path, Right(alg)))))
        }
      }: (CanReadTag, Either[NonEmptyList[ExtractionError], A]))
    }

  /**
    * Returns a function which reads the next tag as Short data from the Coded Input Stream.
    * The returned function returns an error if the value is not a short.
    */
  def shortData[ALG[_]](
    coproductDataDefinition: ALG[Short],
    validations: List[ValidationOp[Short]]): ExtractFromProto[Short] =
    intDataWithFlatMap(coproductDataDefinition, (a, _) => Right(a.toShort), validations)

  /**
    * Returns a function which reads the next tag as an Int from the Coded Input Stream.
    * The returned function returns an ExtractionError if the value is not an Int.
    */
  def intData[ALG[_]](
    coproductDataDefinition: ALG[Int],
    validations: List[ValidationOp[Int]]): ExtractFromProto[Int] =
    intDataWithFlatMap(coproductDataDefinition, identitySuccess, validations)

  def intDataWithFlatMap[ALG[_], A](
    alg: ALG[A],
    f: (Int, Path) => Either[NonEmptyList[ExtractionError], A],
    validations: List[ValidationOp[A]]
  ): ExtractFromProto[A] =
    (fieldNumber: LastFieldNumber, path: Path) => {
      val thisTag = fieldNumber << 3 | VARINT
      (List(thisTag), fieldNumber + 1, (canReadTag, in) => {
        if (in.getLastTag == thisTag) {
          val intData = convert(in, classOf[Int], path)(_.readInt32())
          (true, intData.flatMap(i => f(i, path)).flatMap(i => vu.validate(validations)(i, path)))
        } else {
          (canReadTag, Left(NonEmptyList.one(RequiredValue(path, Right(alg)))))
        }
      }: (CanReadTag, Either[NonEmptyList[ExtractionError], A]))
    }

  def longData[ALG[_]](
    alg: ALG[Long],
    validations: List[ValidationOp[Long]]): ExtractFromProto[Long] =
    longDataWithFlatMap(alg, identitySuccess, validations)

  def longDataWithFlatMap[ALG[_], A](
    alg: ALG[A],
    f: (Long, Path) => Either[NonEmptyList[ExtractionError], A],
    validations: List[ValidationOp[A]]): ExtractFromProto[A] =
    (fieldNumber: LastFieldNumber, path: Path) => {
      val thisTag = fieldNumber << 3 | VARINT
      (List(thisTag), fieldNumber + 1, (canReadTag, in) => {
        if (in.getLastTag == thisTag) {
          val longResult = convert(in, classOf[Long], path)(_.readInt64())
          (
            true,
            longResult.flatMap(l => f(l, path)).flatMap(a => vu.validate(validations)(a, path)))
        } else {
          (canReadTag, Left(NonEmptyList.one(RequiredValue(path, Right(alg)))))
        }
      }: (CanReadTag, Either[NonEmptyList[ExtractionError], A]))
    }

  def byteArrayData[ALG[_], A](
    alg: ALG[A],
    validations: List[ValidationOp[Array[Byte]]]): ExtractFromProto[Array[Byte]] =
    (fieldNumber: LastFieldNumber, path: Path) => {
      val thisField = fieldNumber << 3 | LENGTH_DELIMITED
      (List(thisField), fieldNumber + 1, (canReadTag, in) => {
        if (in.getLastTag == thisField) {
          val result = convert(in, classOf[Array[Byte]], path)(_.readByteArray())
            .flatMap(i => vu.validate(validations)(i, path))
          (true, result)
        } else {
          (canReadTag, Left(NonEmptyList.one(RequiredValue(path, Right(alg)))))
        }
      }: (CanReadTag, Either[NonEmptyList[ExtractionError], Array[Byte]]))
    }

  def timestampWithMap[A](
    f: (Long, Int, Path) => Either[NonEmptyList[ExtractionError], A],
    validations: List[ValidationOp[A]]
  ): ExtractFromProto[A] =
    (fieldNumber: LastFieldNumber, path: Path) => {
      val thisTag = fieldNumber << 3 | LENGTH_DELIMITED
      (List(thisTag), fieldNumber + 1, (canReadTag: CanReadTag, in: CodedInputStream) => {
        val length = in.readRawVarint32()
        val oldLimit = in.pushLimit(length)
        try {
          val dateTimeResult =
            convert(in, classOf[Timestamp], path)(cis => Timestamp.parseFrom(cis))
              .flatMap(timestamp => f(timestamp.getSeconds, timestamp.getNanos, path))
              .flatMap(i => vu.validate(validations)(i, path))
          in.readTag() //should be 0
          in.checkLastTagWas(0)
          in.popLimit(oldLimit)
          (true, dateTimeResult)
        } catch {
          case ex: InvalidProtocolBufferException => {
            in.getLastTag
            (
              canReadTag,
              Left(NonEmptyList.one(WrongTypeError(path, classOf[HList], classOf[Any], Some(ex)))))
          }
          case io: IOException => {
            in.getLastTag
            (
              canReadTag,
              Left(NonEmptyList.one(WrongTypeError(path, classOf[HList], classOf[Any], Some(io)))))
          }
        }
      }: (CanReadTag, Either[NonEmptyList[ExtractionError], A]))
    }

  def floatData[ALG[_], A](
    alg: ALG[A],
    validations: List[ValidationOp[Float]]): ExtractFromProto[Float] =
    (fieldNumber: LastFieldNumber, path: Path) => {
      val thisTag = fieldNumber << 3 | BIT32
      (List(thisTag), fieldNumber + 1, (canReadTag, in) => {
        if (in.getLastTag == thisTag) {
          (
            true,
            convert[Float](in, classOf[Float], path)(_.readFloat()).flatMap(i =>
              vu.validate(validations)(i, path)))
        } else {
          (canReadTag, Left(NonEmptyList.one(RequiredValue(path, Right(alg)))))
        }
      }: (CanReadTag, Either[NonEmptyList[ExtractionError], Float]))
    }

  def doubleData[ALG[_], A](
    coproductDataDefinition: ALG[A],
    validations: List[ValidationOp[Double]]): ExtractFromProto[Double] =
    (fieldNumber: LastFieldNumber, path: Path) => {
      val thisTag = fieldNumber << 3 | BIT64
      (List(thisTag), fieldNumber + 1, (canReadTag, in) => {
        if (in.getLastTag == thisTag) {
          val result = convert(in, classOf[Double], path)(_.readDouble())
            .flatMap(i => vu.validate(validations)(i, path))
          (true, result)
        } else {
          (canReadTag, Left(NonEmptyList.one(RequiredValue(path, Right(coproductDataDefinition)))))
        }
      }: (CanReadTag, Either[cats.data.NonEmptyList[com.bones.data.Error.ExtractionError], Double]))
    }
}

/**
  * Creates a function from Array[Byte]
  */
trait ProtobufSequentialValidatorInterpreter {

  import ProtobufSequentialValidatorInterpreter._

  val zoneOffset: ZoneOffset

  def fromCustomBytes[ALG[_], A](
    dc: PrimitiveWrapperValue[ALG, A],
    customInterpreter: ProtobufValidatorValue[ALG])
    : Array[Byte] => Either[NonEmptyList[ExtractionError], A] = dc match {
    case x: SwitchEncoding[ALG, _, _, A] @unchecked => {
      val (lastFieldNumber, f) =
        kvpHList(x.from, customInterpreter)(1, List.empty)
      (bytes: Array[Byte]) =>
        {
          val is = new ByteArrayInputStream(bytes)
          val cis: CodedInputStream = CodedInputStream.newInstance(is)
          val (_, result) = f(true, cis)
          result.map(o => {
            x.fHtoA(o)
          })
        }: Either[NonEmptyList[ExtractionError], A]
    }
    case _ => ??? //TODO
  }

  protected def kvpCoproduct[ALG[_], C <: Coproduct, A](
    kvp: KvpCoproduct[ALG, C],
    kvpCoproductValue: PrimitiveWrapperValue[ALG, A],
    customInterpreter: ProtobufValidatorValue[ALG]
  ): ExtractProductFromProto[C] = {

    kvp match {
      case nil: KvpCoNil[_] =>
        (lastFieldNumber, path) =>
          (
            List.empty,
            lastFieldNumber,
            (canRead, _) =>
              (canRead, Left(NonEmptyList.one(RequiredValue(path, Left(kvpCoproductValue))))))
      case op: KvpCoproductCollectionHead[ALG, l, r] @unchecked =>
        val vd = determineValueDefinition(op.kvpValue, valueDefinition, customInterpreter)
        (lastFieldNumber, path) =>
          val (headTags, headFieldNumber, fHead) = vd(lastFieldNumber, path)
          val (tailTags, tailFieldNumber, fTail) =
            kvpCoproduct(op.kvpTail, kvpCoproductValue, customInterpreter)(headFieldNumber, path)
          (headTags ::: tailTags, tailFieldNumber, (canReadTag, in) => {
            if (canReadTag) in.readTag()

            if (headTags.contains(in.getLastTag)) {
              val (nextTag, f) = fHead(false, in)
              (nextTag, f.map(Inl(_)))
            } else {
              val (nextTag, f) = fTail(false, in)
              (nextTag, f.map(Inr(_)))
            }
          })
    }
  }

  protected def kvpHList[ALG[_], H <: HList, HL <: Nat](
    group: KvpHListCollection[ALG, H, HL],
    customInterpreter: ProtobufValidatorValue[ALG]): ExtractHListFromProto[H] = {
    group match {
      case nil: KvpNil[_] =>
        (lastFieldNumber, path) =>
          (lastFieldNumber, (canRead, _) => (canRead, Right(HNil)))

      case op: KvpSingleValueHead[ALG, h, t, tl, a] @unchecked =>
        val vd = determineValueDefinition(
          op.fieldDefinition.dataDefinition,
          valueDefinition,
          customInterpreter)
        (lastFieldNumber, path) =>
          {
            val (tag, headFieldNumber, fHead) =
              vd(lastFieldNumber, path :+ op.fieldDefinition.key)
            val (tailFieldNumber, fTail) =
              kvpHList(op.tail, customInterpreter)(headFieldNumber, path)
            (tailFieldNumber, (canReadTag, in) => {
              if (canReadTag) in.readTag

              val (canReadHead, headResult) = fHead(false, in)
              val (canReadTail, tailResult) = fTail(canReadHead, in)

              val result = Util
                .eitherMap2(headResult, tailResult)((l1: h, l2: t) => op.isHCons.cons(l1, l2))
                .flatMap { l =>
                  vu.validate(op.validations)(l, path)
                }
              (canReadTail, result)
            })
          }
      case op: KvpConcreteValueHead[ALG, a, ht, nt] @unchecked =>
        def fromBonesSchema[A](bonesSchema: PrimitiveWrapperValue[ALG, A]): ExtractFromProto[A] =
          ???

        val head = fromBonesSchema(op.collection)
        val tail = kvpHList(op.wrappedEncoding, customInterpreter)
        (lastFieldNumber, path) =>
          {
            val (tags, headFieldNumber, fHead) = head(lastFieldNumber, path)
            val (tailFieldNumber, fTail) = tail(headFieldNumber, path)
            (tailFieldNumber, (canReadTag, in) => {

              val (canReadHead, headResult) = fHead(canReadTag, in)
              val (canReadTail, tailResult) = fTail(canReadHead, in)

              val totalResult = Util
                .eitherMap2(headResult, tailResult)(
                  (l1: a, l2: ht) => op.isHCons.cons(l1, l2)
                )
                .flatMap { l =>
                  vu.validate(op.validations)(l, path)
                }

              (canReadTail, totalResult)
            })
          }
      case op: KvpHListCollectionHead[ALG, a, al, h, hl, t, tl] @unchecked =>
        val head = kvpHList(op.head, customInterpreter)
        val tail = kvpHList(op.tail, customInterpreter)
        (lastFieldNumber, path) =>
          {
            val (headFieldNumber, fHead) = head(lastFieldNumber, path)
            val (tailFieldNumber, fTail) = tail(headFieldNumber, path)
            (tailFieldNumber, (canReadTag, in) => {

              val (canReadHead, headResult) = fHead(canReadTag, in)
              val (canReadTail, tailResult) = fTail(canReadHead, in)

              val totalResult = Util
                .eitherMap2(headResult, tailResult)(
                  (l1: h, l2: t) => {
                    op.prepend(l1, l2)
                  }
                )
                .flatMap { l =>
                  vu.validate[H](op.validations)(l, path)
                }

              (canReadTail, totalResult)
            })
          }
    }
  }

  def valueDefinition[ALG[_], A]
    : (PrimitiveWrapperValue[ALG, A], ProtobufValidatorValue[ALG]) => ExtractFromProto[A] =
    (fgo: PrimitiveWrapperValue[ALG, A], customInterpreter: ProtobufValidatorValue[ALG]) =>
      fgo match {
        case op: OptionalValue[ALG, a] @unchecked =>
          optionalKvpValueDefinition[ALG, a](op, valueDefinition, customInterpreter)
        case ld: ListData[ALG, t] @unchecked =>
          listData[ALG, t](ld, valueDefinition, customInterpreter, ld.validations)
        case ed: EitherData[ALG, a, b] @unchecked =>
          eitherData[ALG, a, b](ed, valueDefinition, valueDefinition, customInterpreter)
        case kvp: CoproductCollection[ALG, c] @unchecked =>
          kvpCoproductValueData[ALG, A, c](kvp, kvpCoproduct, customInterpreter)
        case kvp: KvpCollectionValue[ALG, h, hl] @unchecked =>
          kvpHListValue[ALG, A, h, hl](kvp, kvpHList, customInterpreter)
        case kvp: SwitchEncoding[ALG, h, hl, b] @unchecked =>
          hListConvert[ALG, b, h, hl](kvp, kvpHList, customInterpreter)
        case co: CoproductSwitch[ALG, c, a] @unchecked =>
          kvpCoproductConvert[ALG, c, a](co, kvpCoproduct, customInterpreter)
    }
}
