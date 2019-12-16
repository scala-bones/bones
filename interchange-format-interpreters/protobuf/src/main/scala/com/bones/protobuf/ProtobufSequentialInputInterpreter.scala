package com.bones.protobuf

import java.io.{ByteArrayInputStream, IOException}
import java.time.{LocalDate, LocalDateTime, ZoneOffset}
import java.util.UUID

import cats.data.NonEmptyList
import com.bones.data.Error._
import com.bones.data._
import com.google.protobuf.{CodedInputStream, InvalidProtocolBufferException, Timestamp}
import shapeless.{:+:, CNil, Coproduct, HList, HNil, Inl, Inr, Nat}
import cats.implicits._
import com.bones.Util
import com.bones.data.{KvpCoNil, KvpCoproduct, KvpSingleValueLeft}
import com.bones.syntax.NoAlgebra
import com.bones.validation.{ValidationUtil => vu}

import scala.annotation.tailrec

/**
  * Creates a function from Array[Byte]
  */
object ProtobufSequentialInputInterpreter {

  val zoneOffset = ZoneOffset.UTC

  trait CustomInterpreter[ALG[_]] {
    def extractFromProto[A](alg: ALG[A]): ExtractFromProto[A]
  }

  object NoAlgebraInterpreter extends CustomInterpreter[NoAlgebra] {
    override def extractFromProto[A](alg: NoAlgebra[A]): ExtractFromProto[A] = sys.error("Unreachable code")
  }

  import com.bones.Util._

  type Path = List[String]
  type LastFieldNumber = Int
  type Tag = Int
  type CanReadTag = Boolean
  type ExtractFromProto[A] = (
      LastFieldNumber,
      Path) => (List[Tag],
                LastFieldNumber,
                (CanReadTag, CodedInputStream) => (CanReadTag, Either[NonEmptyList[ExtractionError], A]))
  type ExtractHListFromProto[H <: HList] = (
      LastFieldNumber,
      Path) => (LastFieldNumber,
                (CanReadTag, CodedInputStream) => (CanReadTag, Either[NonEmptyList[ExtractionError], H]))

  type ExtractProductFromProto[C<:Coproduct] = (
    LastFieldNumber,
      Path) => (List[Tag],
                LastFieldNumber,
                (CanReadTag, CodedInputStream) => (CanReadTag, Either[NonEmptyList[ExtractionError], C])
    )

  def fromBytes[A](dc: BonesSchema[NoAlgebra, A]) = fromCustomBytes(dc, NoAlgebraInterpreter)

  def fromCustomBytes[ALG[_], A](dc: BonesSchema[ALG, A], customInterpreter: CustomInterpreter[ALG])
    : Array[Byte] => Either[NonEmptyList[ExtractionError], A] = dc match {
    case x: HListConvert[ALG, _, _, A] @unchecked => {
      val (lastFieldNumber, f) = kvpHList(x.from, customInterpreter)(1, List.empty)
      (bytes: Array[Byte]) =>
        {
          val is = new ByteArrayInputStream(bytes)
          val cis: CodedInputStream = CodedInputStream.newInstance(is)
          val (_, result) = f(true, cis)
          result.map(o => {
            x.fHtoA(o)
          })
        }:Either[NonEmptyList[ExtractionError], A]
    }
  }

  private def kvpCoproduct[ALG[_], C<:Coproduct, A]
    (
      kvp: KvpCoproduct[ALG, C],
      kvpCoproductValue: KvpValue[A],
      customInterpreter: CustomInterpreter[ALG]
    ): ExtractProductFromProto[C] = {

    kvp match {
      case nil: KvpCoNil[_] => (lastFieldNumber, path) =>
        (List.empty, lastFieldNumber, (canRead, _) => (canRead, Left(NonEmptyList.one(RequiredData(path, Left(kvpCoproductValue))))))
      case op: KvpSingleValueLeft[ALG, l,r] @unchecked =>
        val vd = determineValueDefinition(op.kvpValue, customInterpreter)
        (lastFieldNumber, path) =>
          val (headTags, headFieldNumber, fHead) = vd(lastFieldNumber, path)
          val (tailTags, tailFieldNumber, fTail) = kvpCoproduct(op.kvpTail, kvpCoproductValue, customInterpreter)(headFieldNumber, path)
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

  private def kvpHList[ALG[_], H <: HList, HL <: Nat](
      group: KvpHList[ALG, H, HL], customInterpreter: CustomInterpreter[ALG]): ExtractHListFromProto[H] = {
    group match {
      case nil: KvpNil[_] =>
        (lastFieldNumber, path) =>
          (lastFieldNumber, (canRead,_) => (canRead, Right(HNil)))

      case op: KvpSingleValueHead[ALG, h, t, tl, a] @unchecked =>
        val vd = determineValueDefinition(op.fieldDefinition.op, customInterpreter)
        (lastFieldNumber, path) =>
          {
            val (tag, headFieldNumber, fHead) =
              vd(lastFieldNumber, path :+ op.fieldDefinition.key)
            val (tailFieldNumber, fTail) = kvpHList(op.tail, customInterpreter)(headFieldNumber, path)
            (tailFieldNumber, (canReadTag, in) => {
              if (canReadTag) in.readTag

              val (canReadHead, headResult) = fHead(false, in)
              val (canReadTail, tailResult) = fTail(canReadHead, in)

              val result = Util
                .eitherMap2(headResult, tailResult)(
                  (l1: h, l2: t) => op.isHCons.cons(l1,l2))
                .flatMap { l =>
                  vu.validate(op.validations)(l, path)
                }
              (canReadTail, result)
            })
          }
      case op: KvpConcreteTypeHead[ALG, a, ht, nt] @unchecked =>
        def fromBonesSchema[A](bonesSchema: BonesSchema[ALG, A]) : ExtractFromProto[A] = ???

        val head = fromBonesSchema(op.bonesSchema)
        val tail = kvpHList(op.tail, customInterpreter)
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
      case op: KvpHListHead[ALG, a, al, h, hl, t, tl] @unchecked =>
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
                  (l1: h, l2: t) => { op.prepend(l1, l2) }
                )
                .flatMap { l =>
                  vu.validate[H](op.validations)(l, path)
                }

              (canReadTail, totalResult)
            })
          }
    }
  }


  // see https://developers.google.com/protocol-buffers/docs/encoding
  private val VARINT = 0 //	Varint	int32, int64, uint32, uint64, sint32, sint64, bool, enum
  private val BIT64 = 1 // 64-bit	fixed64, sfixed64, double
  private val LENGTH_DELIMITED = 2 // Length-delimited	string, bytes, embedded messages, packed repeated fields
  private val BIT32 = 5 // 32-bit	fixed32, sfixed32, float

  private def determineValueDefinition[ALG[_], A]
  (
    definition: Either[KvpValue[A], ALG[A]],
    customInterpreter: CustomInterpreter[ALG]
  ): ExtractFromProto[A] =
    definition match {
      case Left(kvp) => valueDefinition(kvp, customInterpreter)
      case Right(alg) => customInterpreter.extractFromProto(alg)
    }

  private def valueDefinition[ALG[_], A](
      fgo: KvpValue[A], customInterpreter: CustomInterpreter[ALG]): ExtractFromProto[A] =
    fgo match {
      case op: OptionalKvpValueDefinition[ALG, a] @unchecked =>
        val vd = determineValueDefinition(op.valueDefinitionOp, customInterpreter)
        (fieldNumber: LastFieldNumber, path: Path) =>
          {
            val (tags, childFieldNumber, fa) = vd(fieldNumber, path)
            (tags, childFieldNumber, (canReadInput, in) => {
              if (tags.contains(in.getLastTag)) {
                val (canRead, result) = fa(canReadInput, in)
                (canRead, result.right.map(Some(_)))
              } else {
                (canReadInput, Right(None))
              }
            }:(CanReadTag, Either[NonEmptyList[ExtractionError], A]))
          }
      case ob: BooleanData =>
        (fieldNumber: LastFieldNumber, path: Path) =>
          {
            val thisTag = fieldNumber << 3 | VARINT
            (List(thisTag), fieldNumber + 1, (canReadTag, in) => {
              if (in.getLastTag == thisTag) {
                (true, convert(in, classOf[Boolean], path)(_.readBool()))
              } else {
                (canReadTag, Left(NonEmptyList.one(RequiredData(path, Left(ob)))))
              }
            }:(CanReadTag, Either[NonEmptyList[ExtractionError], Boolean]))
          }
      case rs: StringData =>
        (fieldNumber: LastFieldNumber, path: Path) =>
          {
            val thisTag = fieldNumber << 3 | LENGTH_DELIMITED
            (List(thisTag), fieldNumber + 1, (canReadTag, in) => {
              val lastTag = in.getLastTag
              if (lastTag == thisTag) {
                val utfStringResult = convert(in, classOf[String], path)(cis => {
                  val x = cis.readStringRequireUtf8()
                  x
                })
                (true, utfStringResult)
              } else {
                (canReadTag, Left(NonEmptyList.one(RequiredData(path, Left(rs)))))
              }
            }:(CanReadTag, Either[NonEmptyList[ExtractionError], String]))
          }
      case sd: ShortData =>
        (fieldNumber: LastFieldNumber, path: Path) =>
        {
          val thisTag = fieldNumber << 3 | VARINT
          (List(thisTag), fieldNumber + 1, (canReadTag, in) => {
            if (in.getLastTag == thisTag) {
              (true, convert(in, classOf[Short], path)(_.readInt32().toShort))
            } else {
              (canReadTag, Left(NonEmptyList.one(RequiredData(path, Left(sd)))))
            }
          }:(CanReadTag, Either[NonEmptyList[ExtractionError], Short]))
        }
      case id: IntData =>
        (fieldNumber: LastFieldNumber, path: Path) =>
        {
          val thisTag = fieldNumber << 3 | VARINT
          (List(thisTag), fieldNumber + 1, (canReadTag, in) => {
            if (in.getLastTag == thisTag) {
              (true, convert(in, classOf[Int], path)(_.readInt32()))
            } else {
              (canReadTag, Left(NonEmptyList.one(RequiredData(path, Left(id)))))
            }
          }:(CanReadTag, Either[NonEmptyList[ExtractionError], Int]))
        }
      case ri: LongData =>
        (fieldNumber: LastFieldNumber, path: Path) =>
          {
            val thisTag = fieldNumber << 3 | VARINT
            (List(thisTag), fieldNumber + 1, (canReadTag, in) => {
              if (in.getLastTag == thisTag) {
                (true, convert(in, classOf[Long], path)(_.readInt64()))
              } else {
                (canReadTag, Left(NonEmptyList.one(RequiredData(path, Left(ri)))))
              }
            }:(CanReadTag, Either[NonEmptyList[ExtractionError], Long]))
          }
      case ba: ByteArrayData =>
        (fieldNumber: LastFieldNumber, path: Path) =>
          {
            val thisField = fieldNumber << 3 | LENGTH_DELIMITED
            (List(thisField), fieldNumber + 1, (canReadTag, in) => {
              if (in.getLastTag == thisField) {
                (true, convert(in, classOf[Array[Byte]], path)(_.readByteArray()))
              } else {
                (canReadTag, Left(NonEmptyList.one(RequiredData(path, Left(ba)))))
              }
            }:(CanReadTag, Either[NonEmptyList[ExtractionError], Array[Byte]]))
          }
      case uu: UuidData =>
        (fieldNumber: LastFieldNumber, path: Path) =>
          {
            val thisTag = fieldNumber << 3 | LENGTH_DELIMITED
            (List(thisTag), fieldNumber + 1, (canReadTag, in) => {
              if (in.getLastTag == thisTag) {
                val uuidResult =
                  convert(in, classOf[String], path)(cis => cis.readString())
                  .flatMap(str =>
                    try {
                      Right(UUID.fromString(str))
                    } catch {
                      case arg: IllegalArgumentException =>
                        Left(
                          NonEmptyList.one(
                            CanNotConvert(path, str, classOf[UUID])))
                  })
                (true, uuidResult)
              } else {
                (canReadTag, Left(NonEmptyList.one(RequiredData(path, Left(uu)))))
              }
            }:(CanReadTag, Either[NonEmptyList[ExtractionError], UUID]))
          }
      case dd: LocalDateTimeData =>
        (fieldNumber: LastFieldNumber, path: Path) =>
          {
            val thisTag = fieldNumber  << 3 | LENGTH_DELIMITED
            (List(thisTag), fieldNumber + 1, (canReadTag: CanReadTag, in: CodedInputStream) => {
              val length = in.readRawVarint32()
              val oldLimit = in.pushLimit(length)
              val dateTimeResult = convert(in, classOf[Timestamp], path)(cis => Timestamp.parseFrom(cis))
                .map(timestamp => LocalDateTime.ofEpochSecond(timestamp.getSeconds, timestamp.getNanos, zoneOffset))
              try {
                in.readTag()  //should be 0
                in.checkLastTagWas(0)
                in.popLimit(oldLimit)
                (true, dateTimeResult)
              } catch {
                case ex: InvalidProtocolBufferException => {
                  ex.printStackTrace()
                  in.getLastTag
                  (canReadTag, Left(
                    NonEmptyList.one(
                      WrongTypeError(path, classOf[HList], classOf[Any]))))
                }
              }
            }:(CanReadTag, Either[NonEmptyList[ExtractionError], LocalDateTime]))
          }

      case dt: LocalDateData =>
        (fieldNumber: LastFieldNumber, path: Path) =>
        {
          val thisTag = fieldNumber << 3 | VARINT
          (List(thisTag), fieldNumber + 1, (canReadTag, in) => {
            if (in.getLastTag == thisTag) {
              val localDateResult = convert(in, classOf[Long], path)(_.readInt64())
                .map(LocalDate.ofEpochDay(_))
              (true, localDateResult)
            } else {
              (canReadTag, Left(NonEmptyList.one(RequiredData(path, Left(dt)))))
            }
          }:(CanReadTag, Either[NonEmptyList[ExtractionError], LocalDate]))
        }
      case fd: FloatData =>
        (fieldNumber: LastFieldNumber, path: Path) =>
        {
          val thisTag = fieldNumber << 3 | BIT32
          (List(thisTag), fieldNumber + 1, (canReadTag, in) => {
            if (in.getLastTag == thisTag) {
              (true, convert[Float](in, classOf[Float], path)(_.readFloat()))
            } else {
              (canReadTag, Left(NonEmptyList.one(RequiredData(path, Left(fd)))))
            }
          }:(CanReadTag, Either[NonEmptyList[ExtractionError],Float]))
        }
      case dd: DoubleData =>
        (fieldNumber: LastFieldNumber, path: Path) =>
        {
          val thisTag = fieldNumber << 3 | BIT64
          (List(thisTag), fieldNumber + 1, (canReadTag, in) => {
            if (in.getLastTag == thisTag) {
              (true, convert(in, classOf[Double], path)(_.readDouble()))
            } else {
              (canReadTag, Left(NonEmptyList.one(RequiredData(path, Left(dd)))))
            }
          }:(CanReadTag, Either[cats.data.NonEmptyList[com.bones.data.Error.ExtractionError],Double]))
        }
      case bd: BigDecimalData =>
        (fieldNumber: LastFieldNumber, path: Path) =>
          {
            val thisTag = fieldNumber << 3 | LENGTH_DELIMITED
            (List(thisTag), fieldNumber + 1, (canReadTag, in) => {
              if (in.getLastTag == thisTag) {
                val bigDecimalResult = convert(in, classOf[String], path)(_.readString)
                  .flatMap(stringToBigDecimal(_, path))
                (true, bigDecimalResult)
              } else {
                (canReadTag, Left(NonEmptyList.one(RequiredData(path, Left(bd)))))
              }
            }:(CanReadTag, Either[NonEmptyList[ExtractionError],BigDecimal]))
          }
      case ld: ListData[ALG, t] @unchecked =>
        val child = determineValueDefinition(ld.tDefinition, customInterpreter)
        @tailrec
        def loop[C](
            tags: List[Int],
            canReadListTag: Boolean,
            codedInputStream: CodedInputStream,
            path: List[String],
            accumulated: List[Either[NonEmptyList[ExtractionError], C]],
            f: (CanReadTag, CodedInputStream) => (CanReadTag, Either[NonEmptyList[ExtractionError], C]) )
          : List[Either[NonEmptyList[ExtractionError], C]] = {
          if (canReadListTag) {
            codedInputStream.readTag()
          }
          val lastTag = codedInputStream.getLastTag
          if (! tags.contains(lastTag)) {
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
              val loopResult = loop(tags, canReadTag, in, path, List.empty, f)
              (false, loopResult.sequence) //we read in tag to see the last value
            })
          }
      case ed: EitherData[ALG, a, b] @unchecked =>
        val extractA: ExtractFromProto[a] = determineValueDefinition(ed.definitionA, customInterpreter)
        val extractB: ExtractFromProto[b] = determineValueDefinition(ed.definitionB, customInterpreter)
        (lastFieldNumber, path) => {
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
              (canReadTag, Left(NonEmptyList.one(RequiredData(path, Left(ed)))))
            }
          }:(CanReadTag, Either[NonEmptyList[ExtractionError],A]))

        }
      case esd: EnumerationData[e,a] =>
        (fieldNumber: LastFieldNumber, path: Path) =>
          {
            val thisTag = fieldNumber << 3 | LENGTH_DELIMITED
            (List(thisTag), fieldNumber + 1,
             (canReadTag, in) => {
               val enumeratedResult = convert(in, classOf[String], path)(_.readString)
                 .flatMap(
                   stringToEnumeration[e,a](_,
                     path,
                     esd.enumeration)(
                     esd.manifestOfA))
                 .map(_.asInstanceOf[A])
               (true, enumeratedResult)
             }
            )
          }
      case kvp: KvpCoproductValue[ALG, c] @unchecked => {
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
      case kvp: KvpHListValue[ALG, h, hl] @unchecked => {
        val groupExtract = kvpHList(kvp.kvpHList, customInterpreter)
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
                (true, result.asInstanceOf[Either[NonEmptyList[ExtractionError],A]])
              } catch {
                case ex: InvalidProtocolBufferException => {
                  in.getLastTag
                  (true, Left(
                    NonEmptyList.one(
                      WrongTypeError(path, classOf[HList], classOf[Any]))))
                }
              }
            }:(CanReadTag, Either[NonEmptyList[ExtractionError], A]))
          }
      }
      case kvp: HListConvert[ALG, a, al, b] @unchecked => {
        val groupExtract = kvpHList(kvp.from, customInterpreter)
        (last: LastFieldNumber, path: Path) =>
          {
            val thisTag = last  << 3 | LENGTH_DELIMITED
            val (lastFieldNumber, fIn) = groupExtract(1, path)
            (List(thisTag), last + 1, (canReadTag: CanReadTag, in: CodedInputStream) => {
              val length = in.readRawVarint32()
              val oldLimit = in.pushLimit(length)
              val (_, result) = fIn(true, in)
              try {
                in.readTag()  //should be 0
                in.checkLastTagWas(0)
                in.popLimit(oldLimit)
                (true, result.map(kvp.fHtoA(_)).asInstanceOf[Either[NonEmptyList[ExtractionError], A]])
              } catch {
                case ex: InvalidProtocolBufferException => {
                  ex.printStackTrace()
                  in.getLastTag
                  (canReadTag, Left(
                    NonEmptyList.one(
                      WrongTypeError(path, classOf[HList], classOf[Any]))))
                }
              }
            })
          }
      }
      case co: KvpCoproductConvert[ALG, c,a] @unchecked => {
        val coExtract = kvpCoproduct(co.from, co, customInterpreter)
        (last: LastFieldNumber, path: Path) =>
           {
             val (tags, lastFieldNumber, f) = coExtract(last, path)
             val newF = (canReadTag: CanReadTag, in: CodedInputStream) => {
               val (newCanReadTag, either) = f(canReadTag, in)
               (newCanReadTag, either.map(coproduct => co.cToA(coproduct)))
             }
             (tags, lastFieldNumber, newF)
           }
      }

    }

  private def convert[A](in: CodedInputStream,
                            clazz: Class[A],
                            path: List[String])(f: CodedInputStream => A)
    : Either[NonEmptyList[CanNotConvert[CodedInputStream, A]], A] =
    try {
      Right(f(in))
    } catch {
      case e: IOException => {
        Left(NonEmptyList.one(CanNotConvert(path, in, clazz)))
      }
    }

}
