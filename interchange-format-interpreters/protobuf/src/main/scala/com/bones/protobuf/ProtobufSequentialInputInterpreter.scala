package com.bones.protobuf

import java.io.{ByteArrayInputStream, IOException}
import java.time.{LocalDateTime, ZonedDateTime}
import java.util.UUID

import cats.data.NonEmptyList
import com.bones.data.Error.{CanNotConvert, ExtractionError, RequiredData, WrongTypeError}
import com.bones.data.Value._
import com.google.protobuf.{CodedInputStream, InvalidProtocolBufferException}
import shapeless.{HList, HNil, Nat}
import cats.implicits._
import com.bones.Util
import com.bones.validation.{ValidationUtil => vu}


object ProtobufSequentialInputInterpreter {

  import com.bones.Util._

  type Path = List[String]
  type LastFieldNumber = Int
  type Tag = Int
  type ExtractFromProto[A] = (LastFieldNumber, Path) => (Tag, CodedInputStream => Either[NonEmptyList[ExtractionError],A])
  type ExtractDataClassFromProto[A] = (LastFieldNumber, Path) => (LastFieldNumber, CodedInputStream => Either[NonEmptyList[ExtractionError],A])
  type ExtractHListFromProto[H<:HList] = (LastFieldNumber, Path) => (LastFieldNumber, CodedInputStream => Either[NonEmptyList[ExtractionError],H])

  def fromBytes[A](dc: BonesSchema[A]): Array[Byte] => Either[NonEmptyList[ExtractionError], A] =  dc match {
    case x: HListConvert[_,_,A] => {
      val kvp = kvpHList(x.from)
      (bytes: Array[Byte]) => {
        val kvpResult = kvp(0, List.empty)
        val is = new ByteArrayInputStream(bytes)
        val cis: CodedInputStream = CodedInputStream.newInstance(is)
        val b = kvpResult._2(cis).map(o => {
          x.fab(o)
        })
        b.asInstanceOf[Either[NonEmptyList[ExtractionError],A]]
      }
    }
  }


  private def kvpHList[H<:HList,HL<:Nat](group: KvpHList[H,HL]): ExtractHListFromProto[H] = {
    group match {
      case KvpNil =>
        (lastFieldNumber, path) =>
          (lastFieldNumber, in => Right(HNil))

      case op: KvpSingleValueHead[h, t, tl, a] =>
        val vd = valueDefinition(op.fieldDefinition.op)
        (lastFieldNumber, path) => {
          val headResult = vd(lastFieldNumber, path :+ op.fieldDefinition.key)
          val tailResult = kvpHList(op.tail)(lastFieldNumber + 1, path)
          (tailResult._1, in => {
            val thisTag = in.readTag()

            val result = Util.eitherMap2(headResult._2.apply(in), tailResult._2(in))(
              (l1: h, l2: t) => { l1 :: l2 })
              .flatMap { l =>
                vu.validate(op.validations)(l.asInstanceOf[a],path)
              }
            result
          })
        }
      case op: KvpXMapDataHead[a,ht,nt,ho,xl,xll] =>
        val head = kvpHList(op.xmapData.from)
        val tail = kvpHList(op.tail)
        (lastFieldNumber, path) => {
          val headResult = head(lastFieldNumber, path)
          val tailResult = tail(headResult._1, path)
          (tailResult._1, in => {
            val totalResult = Util.eitherMap2(headResult._2(in), tailResult._2(in))(
              (l1: xl, l2: ht) => { op.xmapData.fab(l1) :: l2 }
            )
              .flatMap { l =>
                vu.validate[ho](op.validations)(l.asInstanceOf[ho],path)
              }

            totalResult
          })
        }
      case op: KvpHListHead[a, al, h, hl, t, tl] =>
        val head = kvpHList(op.head)
        val tail = kvpHList(op.tail)
        (lastFieldNumber, path) => {
          val headResult = head(lastFieldNumber, path)
          val tailResult = tail(headResult._1, path)
          (tailResult._1, in => {
            val totalResult = Util.eitherMap2(headResult._2.apply(in), tailResult._2(in))(
                (l1: h, l2: t) => { op.prepend(l1,l2) }
              )
              .flatMap { l =>
                vu.validate[H](op.validations)(l,path)
              }

            totalResult
          })
        }
    }
  }


  // see https://developers.google.com/protocol-buffers/docs/encoding
  private val VARINT = 0 //	Varint	int32, int64, uint32, uint64, sint32, sint64, bool, enum
  private val BIT64 = 1	// 64-bit	fixed64, sfixed64, double
  private val LENGTH_DELIMITED = 2	// Length-delimited	string, bytes, embedded messages, packed repeated fields
  private val BIT32 = 5	// 32-bit	fixed32, sfixed32, float

  private def valueDefinition[A](fgo: ValueDefinitionOp[A]): ExtractFromProto[A] =
    fgo match {
      case op: OptionalValueDefinition[a] =>
        val vd = valueDefinition(op.valueDefinitionOp)
        (fieldNumber: LastFieldNumber, path: Path) => {
          val (tag, fa) = vd(fieldNumber, path)
          (tag, in => {
            val result = if (in.getLastTag == tag) {
              fa(in).right.map(Some(_))
            } else {
              Right(None)
            }
            result.asInstanceOf[Either[NonEmptyList[ExtractionError],A]]
          })
        }
      case ob: BooleanData =>
        (fieldNumber: LastFieldNumber, path: Path) => {
          val thisField = (fieldNumber + 1) << 3 | VARINT
          (thisField, in => {
            val result = if (in.getLastTag == thisField) {
              convert(in, classOf[Boolean], path)(_.readBool())
            } else {
              Left(NonEmptyList.one(RequiredData(path, ob)))
            }
            result.asInstanceOf[Either[NonEmptyList[ExtractionError],Boolean]]
          })
        }
      case rs: StringData =>
        (fieldNumber: LastFieldNumber, path: Path) => {
          val thisField = (fieldNumber + 1) << 3 | LENGTH_DELIMITED
          (thisField, in => {
            val lastTag = in.getLastTag
            val result = if (lastTag == thisField) {
              convert(in, classOf[String], path)(cis => {
                val x = cis.readStringRequireUtf8()
                x
              })
            } else {
              Left(NonEmptyList.one(RequiredData(path, rs)))
            }
            result.asInstanceOf[Either[NonEmptyList[ExtractionError],String]]
          })
        }
      case ri: LongData =>
        (fieldNumber: LastFieldNumber, path: Path) => {
          val thisField = (fieldNumber + 1) << 3 | VARINT
          (thisField, in => {
            val result = if (in.getLastTag == thisField) {
              convert(in, classOf[Long], path)(_.readInt64()).asInstanceOf[Either[NonEmptyList[ExtractionError],A]]
            } else {
              Left(NonEmptyList.one(RequiredData(path, ri)))
            }
            result.asInstanceOf[Either[NonEmptyList[ExtractionError],Long]]
          })
        }
      case ba: ByteArrayData =>
        (fieldNumber: LastFieldNumber, path: Path) => {
          val thisField = (fieldNumber + 1) << 3 | LENGTH_DELIMITED
          (thisField, in => {
            val result = if (in.getLastTag == thisField) {
              convert(in, classOf[Array[Byte]], path)(_.readByteArray()).asInstanceOf[Either[NonEmptyList[ExtractionError],A]]
            } else {
              Left(NonEmptyList.one(RequiredData(path, ba)))
            }
            result.asInstanceOf[Either[NonEmptyList[ExtractionError],Array[Byte]]]
          })
        }
      case uu: UuidData =>
        (fieldNumber: LastFieldNumber, path: Path) => {
          val thisField = (fieldNumber + 1) << 3 | LENGTH_DELIMITED
          (thisField, in => {
            val result = if (in.getLastTag == thisField) {
              convert(in, classOf[String], path)(cis => cis.readString())
                .flatMap(str => try {
                  Right(UUID.fromString(str))
                } catch {
                  case arg: IllegalArgumentException => Left(NonEmptyList.one(CanNotConvert(path, str, classOf[UUID])))
                })
            } else {
              Left(NonEmptyList.one(RequiredData(path, uu)))
            }
            result.asInstanceOf[Either[NonEmptyList[ExtractionError],UUID]]
          })
        }
      case dd: DateTimeData =>
        (fieldNumber: LastFieldNumber, path: Path) => {
          val thisField = (fieldNumber + 1) << 3 | VARINT
          (thisField, in => {
            val result = if (in.getLastTag == thisField) {
              convert(in, classOf[String], path)(_.readString).flatMap(stringToZonedDateTime(_,dd.dateFormat, path))
            } else {
              Left(NonEmptyList.one(RequiredData(path, dd)))
            }
            result.asInstanceOf[Either[NonEmptyList[ExtractionError],ZonedDateTime]]
          })
        }
      case bd: BigDecimalData =>
        (fieldNumber: LastFieldNumber, path: Path) => {
          val thisField = (fieldNumber + 1) << 3 | LENGTH_DELIMITED
          (thisField, in => {
            val result = if (in.getLastTag == thisField) {
              convert(in, classOf[String], path)(_.readString).flatMap(stringToBigDecimal(_,path))
            } else {
              Left(NonEmptyList.one(RequiredData(path, bd)))
            }
            result.asInstanceOf[Either[NonEmptyList[ExtractionError],BigDecimal]]
          })
        }
      case ld: ListData[t] =>
        val child = valueDefinition(ld.tDefinition)
        def loop[C](thisField: Int,
                    codedInputStream: CodedInputStream,
                    path: List[String],
                    f: CodedInputStream => Either[NonEmptyList[ExtractionError],C]
                   ) : List[Either[NonEmptyList[ExtractionError],C]] = {
          val lastTag = codedInputStream.getLastTag
          if (lastTag != thisField) {
            List.empty
          } else {
            val result = f(codedInputStream)
            codedInputStream.readTag()
            result :: loop(thisField, codedInputStream, path, f)
          }
        }

        (fieldNumber: LastFieldNumber, path: Path) => {
          val (field, f) = child.apply(fieldNumber, path)
          (field, in => loop(field, in, path, f).sequence)
        }
      case ed: EitherData[a,b] => ??? // use one of, punting on this for a bit
//        val a = valueDefinition(ed.definitionA)
//        val b = valueDefinition(ed.definitionB)
//        (false, EitherType(a._2,b._2), (in, path) => ???)
      case esd: EnumerationStringData[a] =>
        (fieldNumber: LastFieldNumber, path: Path) => {

          val thisField = (fieldNumber + 1) << 3 | LENGTH_DELIMITED
          (thisField, in =>
            convert(in, classOf[String], path)(_.readString)
              .flatMap(stringToEnumeration(_,path,esd.enumeration, esd.manifestOfA))
              .asInstanceOf[Either[NonEmptyList[ExtractionError],A]]
          )
        }
      case sum: SumTypeData[a,b] => {
        val f = valueDefinition(sum.from)
        (last: LastFieldNumber, path: Path) => {
          val result = f(last, path)
          val fCis = (cis: CodedInputStream) => {
            result._2(cis).flatMap(a => sum.fab(a,path).left.map(x => NonEmptyList.one(x)))
          }
          (result._1, fCis)
        }
      }
      case kvp: KvpHListValue[h,hl] => {
        val groupExtract = kvpHList(kvp.kvpHList)
        (last: LastFieldNumber, path: Path) => {
          val thisField = (last + 1) << 3 | LENGTH_DELIMITED
          val children = groupExtract.apply(0, path)
          (thisField, (in: CodedInputStream) => {
            val length    = in.readRawVarint32()
            val oldLimit  = in.pushLimit(length)
            val result = children._2(in)
            try {
              in.checkLastTagWas(0)
              in.popLimit(oldLimit)
              result.asInstanceOf[Either[NonEmptyList[ExtractionError], A]]
            } catch {
              case ex: InvalidProtocolBufferException => {
                in.getLastTag
                Left(NonEmptyList.one(WrongTypeError(path, classOf[HList], classOf[Any])))
              }
            }
          })
        }
      }
      case kvp: HListConvert[a, al, b] => {
        val groupExtract = kvpHList(kvp.from)
        (last: LastFieldNumber, path: Path) => {
          val thisField = (last + 1) << 3 | LENGTH_DELIMITED
          val children = groupExtract.apply(0, path)
          (thisField, (in: CodedInputStream) => {
            val length    = in.readRawVarint32()
            val oldLimit  = in.pushLimit(length)
            val result = children._2(in).map(kvp.fab(_))
            try {
              in.readTag()
              in.checkLastTagWas(0)
              in.popLimit(oldLimit)
              result.asInstanceOf[Either[NonEmptyList[ExtractionError], A]]
            } catch {
              case ex: InvalidProtocolBufferException => {
                in.getLastTag
                Left(NonEmptyList.one(WrongTypeError(path, classOf[HList], classOf[Any])))
              }
            }
          })
        }
      }
    }


  private def convert[A,T](in: CodedInputStream, clazz: Class[A], path: List[String])(f: CodedInputStream => A): Either[NonEmptyList[CanNotConvert[CodedInputStream,A]],A] =
    try {
      Right(f(in))
    } catch {
      case e: IOException => {
        Left(NonEmptyList.one(CanNotConvert(path, in, clazz)))
      }
    }

}
