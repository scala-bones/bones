package com.bones.protobuf

import java.io.{ByteArrayInputStream, IOException}
import java.time.{LocalDateTime, ZoneOffset}

import com.bones.{Path, Util, data}
import com.bones.data.Error._
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

  def identitySuccess[A]: (A, Path[String]) => Either[ExtractionErrors[String], A] =
    (a, _) => Right(a)

  /** Returns a function reads boolean data from the codedInputStream */
  def booleanData[ALG[_], A](validations: List[ValidationOp[Boolean]]): ExtractFromProto[Boolean] =
    (fieldNumber: LastFieldNumber, path: Path[String]) => {
      val thisTag = fieldNumber << 3 | VARINT
      (List(thisTag), fieldNumber + 1, (canReadTag: CanReadTag, in: CodedInputStream) => {
        if (in.getLastTag == thisTag) {
          (
            true,
            convert(in, classOf[Boolean], path)(_.readBool()).flatMap(bool =>
              vu.validate(validations)(bool, path)))
        } else {
          (canReadTag, Left(List(RequiredValue(path, "Boolean"))))
        }
      }: (CanReadTag, Either[ExtractionErrors[String], Boolean]))
    }

  /** Returns a function which reads String data as UTF from the CodedInputStream */
  def stringData[ALG[_]](validations: List[ValidationOp[String]]): ExtractFromProto[String] =
    stringDataWithFlatMap("String", identitySuccess, validations)

  def stringDataWithFlatMap[ALG[_], A](
    typeName: String,
    f: (String, Path[String]) => Either[ExtractionErrors[String], A],
    validations: List[ValidationOp[A]]): ExtractFromProto[A] =
    (fieldNumber: LastFieldNumber, path: Path[String]) => {
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
          (canReadTag, Left(List(RequiredValue(path, typeName))))
        }
      }: (CanReadTag, Either[ExtractionErrors[String], A]))
    }

  /**
    * Returns a function which reads the next tag as Short data from the Coded Input Stream.
    * The returned function returns an error if the value is not a short.
    */
  def shortData[ALG[_]](validations: List[ValidationOp[Short]]): ExtractFromProto[Short] =
    intDataWithFlatMap((a, _) => Right(a.toShort), validations)

  /**
    * Returns a function which reads the next tag as an Int from the Coded Input Stream.
    * The returned function returns an ExtractionError if the value is not an Int.
    */
  def intData[ALG[_]](validations: List[ValidationOp[Int]]): ExtractFromProto[Int] =
    intDataWithFlatMap(identitySuccess, validations)

  def intDataWithFlatMap[ALG[_], A](
    f: (Int, Path[String]) => Either[ExtractionErrors[String], A],
    validations: List[ValidationOp[A]]
  ): ExtractFromProto[A] =
    (fieldNumber: LastFieldNumber, path: Path[String]) => {
      val thisTag = fieldNumber << 3 | VARINT
      (List(thisTag), fieldNumber + 1, (canReadTag, in) => {
        if (in.getLastTag == thisTag) {
          val intData = convert(in, classOf[Int], path)(_.readInt32())
          (true, intData.flatMap(i => f(i, path)).flatMap(i => vu.validate(validations)(i, path)))
        } else {
          (canReadTag, Left(List(RequiredValue(path, "Int"))))
        }
      }: (CanReadTag, Either[ExtractionErrors[String], A]))
    }

  def longData[ALG[_]](validations: List[ValidationOp[Long]]): ExtractFromProto[Long] =
    longDataWithFlatMap(identitySuccess, validations)

  def longDataWithFlatMap[ALG[_], A](
    f: (Long, Path[String]) => Either[ExtractionErrors[String], A],
    validations: List[ValidationOp[A]]): ExtractFromProto[A] =
    (fieldNumber: LastFieldNumber, path: Path[String]) => {
      val thisTag = fieldNumber << 3 | VARINT
      (List(thisTag), fieldNumber + 1, (canReadTag, in) => {
        if (in.getLastTag == thisTag) {
          val longResult = convert(in, classOf[Long], path)(_.readInt64())
          (
            true,
            longResult.flatMap(l => f(l, path)).flatMap(a => vu.validate(validations)(a, path)))
        } else {
          (canReadTag, Left(List(RequiredValue(path, "Long"))))
        }
      }: (CanReadTag, Either[ExtractionErrors[String], A]))
    }

  def byteArrayData[ALG[_], A](
    validations: List[ValidationOp[Array[Byte]]]): ExtractFromProto[Array[Byte]] =
    (fieldNumber: LastFieldNumber, path: Path[String]) => {
      val thisField = fieldNumber << 3 | LENGTH_DELIMITED
      (List(thisField), fieldNumber + 1, (canReadTag, in) => {
        if (in.getLastTag == thisField) {
          val result = convert(in, classOf[Array[Byte]], path)(_.readByteArray())
            .flatMap(i => vu.validate(validations)(i, path))
          (true, result)
        } else {
          (canReadTag, Left(List(RequiredValue(path, "arrayOfByte"))))
        }
      }: (CanReadTag, Either[ExtractionErrors[String], Array[Byte]]))
    }

  def timestampWithMap[A](
    f: (Long, Int, Path[String]) => Either[ExtractionErrors[String], A],
    validations: List[ValidationOp[A]]
  ): ExtractFromProto[A] =
    (fieldNumber: LastFieldNumber, path: Path[String]) => {
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
            (canReadTag, Left(List(WrongTypeError(path, "Timestamp", "Unknown", Some(ex)))))
          }
          case io: IOException => {
            in.getLastTag
            (canReadTag, Left(List(WrongTypeError(path, "Timestamp", "Unknown", Some(io)))))
          }
        }
      }: (CanReadTag, Either[ExtractionErrors[String], A]))
    }

  def floatData[ALG[_], A](validations: List[ValidationOp[Float]]): ExtractFromProto[Float] =
    (fieldNumber: LastFieldNumber, path: Path[String]) => {
      val thisTag = fieldNumber << 3 | BIT32
      (List(thisTag), fieldNumber + 1, (canReadTag, in) => {
        if (in.getLastTag == thisTag) {
          (
            true,
            convert[Float](in, classOf[Float], path)(_.readFloat()).flatMap(i =>
              vu.validate(validations)(i, path)))
        } else {
          (canReadTag, Left(List(RequiredValue(path, "Float"))))
        }
      }: (CanReadTag, Either[ExtractionErrors[String], Float]))
    }

  def doubleData[ALG[_], A](validations: List[ValidationOp[Double]]): ExtractFromProto[Double] =
    (fieldNumber: LastFieldNumber, path: Path[String]) => {
      val thisTag = fieldNumber << 3 | BIT64
      (List(thisTag), fieldNumber + 1, (canReadTag, in) => {
        if (in.getLastTag == thisTag) {
          val result = convert(in, classOf[Double], path)(_.readDouble())
            .flatMap(i => vu.validate(validations)(i, path))
          (true, result)
        } else {
          (canReadTag, Left(List(RequiredValue(path, "Double"))))
        }
      }: (CanReadTag, Either[ExtractionErrors[String], Double]))
    }

  def convert[A](
    in: CodedInputStream,
    clazz: Class[A],
    path: List[String]
  )(f: CodedInputStream => A): Either[List[CanNotConvert[String, CodedInputStream, A]], A] =
    try {
      Right(f(in))
    } catch {
      case e: IOException =>
        Left(List(CanNotConvert(path, in, clazz, Some(e))))
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
    definition: Either[HigherOrderValue[String, ALG, A], ALG[A]]
  ): ExtractFromProto[A] =
    definition match {
      case Left(kvp) => valueDefinition(key, kvp)
      case Right(alg) =>
        (lastItem, path) =>
          customInterpreter.extractFromProto(alg).apply(lastItem, key :: path)
    }

  val zoneOffset: ZoneOffset

  def fromCustomBytes[A](
    dc: KvpCollection[String, ALG, A]): Array[Byte] => Either[ExtractionErrors[String], A] = {
    val (_, _, f) =
      fromKvpCollection(dc)(1, List.empty)
    (bytes: Array[Byte]) =>
      {
        val is = new ByteArrayInputStream(bytes)
        val cis: CodedInputStream = CodedInputStream.newInstance(is)
        f(true, cis)._2
      }: Either[ExtractionErrors[String], A]
  }

  protected def kvpCoproduct[C <: Coproduct, A](
    kvp: KvpCoproduct[String, ALG, C]
  ): ExtractFromProto[C] = {

    kvp match {
      case _: KvpCoNil[String, _] @unchecked =>
        (lastFieldNumber, path) =>
          (
            List.empty,
            lastFieldNumber,
            (canRead, _) =>
              (canRead, Left(List(RequiredValue(path, s"coproduct ${kvp} not found")))))
      case op: KvpCoproductCollectionHead[String, ALG, a, c, C] @unchecked =>
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

  def fromKvpCollection[A](group: KvpCollection[String, ALG, A]): ExtractFromProto[A] = {
    group match {
      case _: KvpNil[String, ALG] =>
        (lastFieldNumber, path) =>
          (List.empty, lastFieldNumber, (canRead, _) => (canRead, Right(HNil)))

      case op: KvpSingleValueHead[String, ALG, h, t, tl, a] @unchecked =>
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
      case op: KvpHListCollectionHead[String, ALG, ho, no, h, hl, t, tl] @unchecked =>
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
      case op: KvpCoproduct[String, ALG, c] => kvpCoproduct(op).asInstanceOf[ExtractFromProto[A]]
      case op: KvpWrappedCoproduct[String, ALG, a, c] =>
        val groupF = fromKvpCollection(op.wrappedEncoding)
        unwrap(groupF, op.fCtoA)
      case op: KvpWrappedHList[String, ALG, a, xs, xsl] =>
        val groupF = fromKvpCollection(op.wrappedEncoding)
        unwrap(groupF, op.fHtoA)
    }

  }

  private def unwrap[A, B](extractF: ExtractFromProto[A], fAtoB: A => B): ExtractFromProto[B] = {
    (last: LastFieldNumber, path: Path[String]) =>
      {
        val (tags, lastFieldNumber, childF) = extractF(last, path)
        val f = (canReadInput: CanReadTag, in: CodedInputStream) => {
          val (canRead, result) = childF(canReadInput, in)
          (canRead, result.map(cResult => fAtoB(cResult)))
        }
        (tags, lastFieldNumber, f)
      }
  }

  def valueDefinition[A](key: String, fgo: HigherOrderValue[String, ALG, A]): ExtractFromProto[A] =
    fgo match {
      case op: OptionalValue[String, ALG, a] @unchecked =>
        optionalKvpValueDefinition[a](key, op)
      case ld: ListData[String, ALG, t] @unchecked =>
        listData[t](key, ld)
      case ed: EitherData[String, ALG, a, b] @unchecked =>
        eitherData[a, b](key, ed)
      case kvp: KvpCollectionValue[String, ALG, A] @unchecked => {
        val groupExtract = fromKvpCollection[A](kvp.kvpCollection)
        (last: LastFieldNumber, path: Path[String]) =>
          {
            val thisTag = last << 3 | LENGTH_DELIMITED
            val (tags, lastFieldNumber, fIn) = groupExtract(1, path)
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
                    .asInstanceOf[Either[ExtractionErrors[String], A]]
                    .flatMap(i => vu.validate(kvp.validations)(i, path)))
              } catch {
                case ex: InvalidProtocolBufferException => {
                  in.getLastTag
                  (canReadTag, Left(List(WrongTypeError(path, kvp.typeName, "Unknown", Some(ex)))))
                }
              }
            })
          }
      }
    }

  def optionalKvpValueDefinition[B](
    key: String,
    op: OptionalValue[String, ALG, B]): ExtractFromProto[Option[B]] = {
    val vd = determineValueDefinition(key, op.valueDefinitionOp)
    (fieldNumber: LastFieldNumber, path: Path[String]) =>
      {
        val (tags, childFieldNumber, fa) = vd(fieldNumber, path)

        (tags, childFieldNumber, (canReadInput, in) => {
          if (tags.contains(in.getLastTag)) {
            val (canRead, result) = fa(canReadInput, in)
            (canRead, result.map(Some(_)))
          } else {
            (canReadInput, Right(None))
          }
        }: (CanReadTag, Either[ExtractionErrors[String], Option[B]]))
      }
  }

  def listData[B](key: String, ld: ListData[String, ALG, B]): ExtractFromProto[List[B]] = {
    val child = determineValueDefinition[B](key, ld.tDefinition)

    @tailrec
    def loop[C](
      tags: List[Int],
      canReadListTag: Boolean,
      codedInputStream: CodedInputStream,
      path: List[String],
      accumulated: List[Either[ExtractionErrors[String], C]],
      f: (CanReadTag, CodedInputStream) => (CanReadTag, Either[ExtractionErrors[String], C])
    ): List[Either[ExtractionErrors[String], C]] = {
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

    (fieldNumber: LastFieldNumber, path: Path[String]) =>
      {
        val (tags, lastFieldNumber, f) = child(fieldNumber, path)
        (tags, lastFieldNumber, (canReadTag, in) => {
          val loopResult =
            Util
              .sequence(loop(tags, canReadTag, in, path, List.empty, f))
              .flatMap(i => vu.validate(ld.validations)(i, path))
          (false, loopResult) //we read in tag to see the last value
        })
      }
  }

  def eitherData[B, C](
    key: String,
    ed: EitherData[String, ALG, B, C]
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
            (canReadTag, Left(List(RequiredValue(path, ed.typeName))))
          }
        }: (CanReadTag, Either[ExtractionErrors[String], Either[B, C]]))
      }
  }

  def newGroup[A](extract: ExtractFromProto[A]): ExtractFromProto[A] = {
    (last: LastFieldNumber, path: Path[String]) =>
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
            (true, result.asInstanceOf[Either[ExtractionErrors[String], A]])
          } catch {
            case ex: InvalidProtocolBufferException => {
              in.getLastTag
              (true, Left(List(WrongTypeError(path, "Object", "Unknown", Some(ex)))))
            }
          }
        }: (CanReadTag, Either[ExtractionErrors[String], A]))
      }
  }

}
