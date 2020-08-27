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

  // For FieldNumber definitions
  // see https://developers.google.com/protocol-buffers/docs/encoding
  private val VARINT = 0 //	Varint	int32, int64, uint32, uint64, sint32, sint64, bool, enum
  private val BIT64 = 1 // 64-bit	fixed64, sfixed64, double
  private val LENGTH_DELIMITED = 2 // Length-delimited	string, bytes, embedded messages, packed repeated fields
  private val BIT32 = 5 // 32-bit	fixed32, sfixed32, float

  def identitySuccess[A]: (A, Path) => Either[NonEmptyList[ExtractionError], A] = (a, _) => Right(a)

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
          (
            canReadTag,
            Left(NonEmptyList.one(RequiredValue.fromDef(path, Right(coproductDataDefinition)))))
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
          (canReadTag, Left(NonEmptyList.one(RequiredValue.fromDef(path, Right(alg)))))
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
          (canReadTag, Left(NonEmptyList.one(RequiredValue.fromDef(path, Right(alg)))))
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
          (canReadTag, Left(NonEmptyList.one(RequiredValue.fromDef(path, Right(alg)))))
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
          (canReadTag, Left(NonEmptyList.one(RequiredValue.fromDef(path, Right(alg)))))
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
          (canReadTag, Left(NonEmptyList.one(RequiredValue.fromDef(path, Right(alg)))))
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
          (
            canReadTag,
            Left(NonEmptyList.one(RequiredValue.fromDef(path, Right(coproductDataDefinition)))))
        }
      }: (CanReadTag, Either[cats.data.NonEmptyList[com.bones.data.Error.ExtractionError], Double]))
    }

  def convert[A](
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

}

/**
  * Creates a function from Array[Byte]
  */
trait ProtobufSequentialValidatorInterpreter[ALG[_]] {

  import ProtobufSequentialValidatorInterpreter._

  val customInterpreter: ProtobufValidatorValue[ALG]

  /** Determine if we use the core algebra or the custom algebra and then pass control to the appropriate interpreter */
  def determineValueDefinition[A](
    key: String,
    definition: Either[PrimitiveWrapperValue[ALG, A], ALG[A]]
  ): ExtractFromProto[A] =
    definition match {
      case Left(kvp) => valueDefinition(key, kvp)
      case Right(alg) =>
        (lastItem, path) =>
          customInterpreter.extractFromProto(alg).apply(lastItem, key :: path)
    }

  val zoneOffset: ZoneOffset

  def fromCustomBytes[A](
    dc: KvpCollectionValue[ALG, A]): Array[Byte] => Either[NonEmptyList[ExtractionError], A] = {
    val (_, _, f) =
      fromKvpCollection(dc.kvpCollection)(1, List.empty)
    (bytes: Array[Byte]) =>
      {
        val is = new ByteArrayInputStream(bytes)
        val cis: CodedInputStream = CodedInputStream.newInstance(is)
        f(true, cis)._2
      }: Either[NonEmptyList[ExtractionError], A]
  }

  protected def kvpCoproduct[C <: Coproduct, A](
    kvp: KvpCoproduct[ALG, C]
  ): ExtractFromProto[C] = {

    kvp match {
      case _: KvpCoNil[_] =>
        (lastFieldNumber, path) =>
          (
            List.empty,
            lastFieldNumber,
            (canRead, _) =>
              (canRead, Left(NonEmptyList.one(RequiredValue(path, s"coproduct ${kvp} not found")))))
      case op: KvpCoproductCollectionHead[ALG, a, c, C] @unchecked =>
        val head = fromKvpCollection(op.kvpCollection)
        (lastFieldNumber, path) =>
          val (headTags, headFieldNumber, fHead) = head(lastFieldNumber, path)
          val (tailTags, tailFieldNumber, fTail) =
            kvpCoproduct(op.kvpTail)(headFieldNumber, path)
          (headTags ::: tailTags, tailFieldNumber, (canReadTag, in) => {
            if (canReadTag) in.readTag()

            if (headTags.contains(in.getLastTag)) {
              val (nextTag, f) = fHead(false, in)
              (nextTag, f.map(Inl(_).asInstanceOf[C]))
            } else {
              val (nextTag, f) = fTail(false, in)
              (nextTag, f.map(Inr(_).asInstanceOf[C]))
            }
          })
    }
  }

  def fromKvpCollection[A](group: KvpCollection[ALG, A]): ExtractFromProto[A] = {
    group match {
      case _: KvpNil[ALG] =>
        (lastFieldNumber, path) =>
          (List.empty, lastFieldNumber, (canRead, _) => (canRead, Right(HNil)))

      case op: KvpSingleValueHead[ALG, h, t, tl, a] @unchecked =>
        val headF = op.head match {
          case Left(keyValueDef) =>
            determineValueDefinition(keyValueDef.key, keyValueDef.dataDefinition)
          case Right(kvpCollection) => fromKvpCollection(kvpCollection)
        }
        (lastFieldNumber, path) =>
          {
            val (tag, headFieldNumber, fHead) =
              headF(lastFieldNumber, path)
            val (tailTags, tailFieldNumber, fTail) =
              fromKvpCollection(op.tail)(headFieldNumber, path)
            (tag ::: tailTags, tailFieldNumber, (canReadTag, in) => {
              if (canReadTag) in.readTag

              val (canReadHead, headResult) = fHead(false, in)
              val (canReadTail, tailResult) = fTail(canReadHead, in)

              val result = Util
                .eitherMap2(headResult, tailResult)((l1: h, l2: t) => op.isHCons.cons(l1, l2))
                .flatMap { l =>
                  vu.validate(op.validations)(l, path)
                }
              (canReadTail, result.map(_.asInstanceOf[A]))
            })
          }
      case op: KvpHListCollectionHead[ALG, ho, no, h, hl, t, tl] @unchecked =>
        val head = fromKvpCollection(op.head)
        val tail = fromKvpCollection(op.tail)
        (lastFieldNumber, path) =>
          {
            val (tags, headFieldNumber, fHead) = head(lastFieldNumber, path)
            val (tailTags, tailFieldNumber, fTail) = tail(headFieldNumber, path)
            (tailTags, tailFieldNumber, (canReadTag, in) => {

              val (canReadHead, headResult) = fHead(canReadTag, in)
              val (canReadTail, tailResult) = fTail(canReadHead, in)

              val totalResult = Util
                .eitherMap2(headResult, tailResult)(
                  (l1: h, l2: t) => op.prepend(l1, l2)
                )
                .flatMap { l =>
                  vu.validate(op.validations)(l, path)
                }

              (canReadTail, totalResult.map(_.asInstanceOf[A]))
            })
          }
      case op: KvpCoproduct[ALG, c] => kvpCoproduct(op).asInstanceOf[ExtractFromProto[A]]
      case op: KvpWrappedCoproduct[ALG, a, c] =>
        val groupF = fromKvpCollection(op.wrappedEncoding)
        unwrap(groupF, op.fCtoA)
      case op: KvpWrappedHList[ALG, a, xs, xsl] =>
        val groupF = fromKvpCollection(op.wrappedEncoding)
        unwrap(groupF, op.fHtoA)
    }

  }

  private def unwrap[A, B](extractF: ExtractFromProto[A], fAtoB: A => B): ExtractFromProto[B] = {
    (last: LastFieldNumber, path: Path) =>
      {
        val (tags, lastFieldNumber, childF) = extractF(last, path)
        val f = (canReadInput: CanReadTag, in: CodedInputStream) => {
          val (canRead, result) = childF(canReadInput, in)
          (canRead, result.map(cResult => fAtoB(cResult)))
        }
        (tags, lastFieldNumber, f)
      }
  }

  def valueDefinition[A](key: String, fgo: PrimitiveWrapperValue[ALG, A]): ExtractFromProto[A] =
    fgo match {
      case op: OptionalValue[ALG, a] @unchecked =>
        optionalKvpValueDefinition[a](key, op)
      case ld: ListData[ALG, t] @unchecked =>
        listData[t](key, ld)
      case ed: EitherData[ALG, a, b] @unchecked =>
        eitherData[a, b](key, ed)
      case kvp: KvpCollectionValue[ALG, A] @unchecked => {
        val f = fromKvpCollection[A](kvp.kvpCollection)
        (lastFieldNumber, path) =>
          f(lastFieldNumber, key :: path)
      }
    }

  def optionalKvpValueDefinition[B](
    key: String,
    op: OptionalValue[ALG, B]): ExtractFromProto[Option[B]] = {
    val vd = determineValueDefinition(key, op.valueDefinitionOp)
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

  def listData[B](key: String, ld: ListData[ALG, B]): ExtractFromProto[List[B]] = {
    val child = determineValueDefinition[B](key, ld.tDefinition)

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
            .flatMap(i => vu.validate(ld.validations)(i, path))
          (false, loopResult) //we read in tag to see the last value
        })
      }
  }

  def eitherData[B, C](
    key: String,
    ed: EitherData[ALG, B, C]
  ): ExtractFromProto[Either[B, C]] = {
    val extractA: ExtractFromProto[B] =
      determineValueDefinition[B](key, ed.definitionA)
    val extractB: ExtractFromProto[C] =
      determineValueDefinition[C](key, ed.definitionB)
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
            (canReadTag, Left(NonEmptyList.one(RequiredValue.fromDef(path, Left(ed)))))
          }
        }: (CanReadTag, Either[NonEmptyList[ExtractionError], Either[B, C]]))

      }
  }

  def newGroup[A](extract: ExtractFromProto[A]): ExtractFromProto[A] = {
    (last: LastFieldNumber, path: Path) =>
      {
        val tag = (last + 1) << 3 | LENGTH_DELIMITED
        val (tags, lastFieldNumber, f) = extract.apply(0, path)
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

}
