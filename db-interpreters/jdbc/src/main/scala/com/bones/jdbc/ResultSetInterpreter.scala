package com.bones.jdbc

import java.sql.{ResultSet, SQLException}
import java.time.{LocalDate, LocalDateTime, ZoneId}
import java.util.Date

import cats.data.NonEmptyList
import com.bones.Util
import com.bones.Util.{stringToEnumeration, stringToUuid}
import com.bones.data.Error.{ExtractionError, RequiredData, SystemError}
import com.bones.data._
import DbUtil.camelToSnake
import FindInterpreter.{FieldName, Path, utcCalendar}
import shapeless.{HList, HNil, Nat}

/** Responsible for converting a result set into the result type */
trait ResultSetInterpreter {

  def forListData[T](kvpHList: ListData[T])

  def kvpHList[H <: HList, N <: Nat](group: KvpHList[H, N])
    : Path => ResultSet => Either[NonEmptyList[ExtractionError], H] =
    group match {
      case KvpNil =>
        path => rs =>
          Right(HNil)
      case op: KvpSingleValueHead[h, t, tl, a] =>
        path =>
          {
            val newPath = op.fieldDefinition.key :: path
            val rsToHead = valueDefinition(op.fieldDefinition.op)(
              newPath,
              camelToSnake(op.fieldDefinition.key))
            val rsToTail = kvpHList(op.tail)(path)
            rs =>
              {
                Util.eitherMap2(rsToHead(rs), rsToTail(rs))((l1: h, l2: t) => {
                  op.isHCons.cons(l1,l2)
                })
              }
          }
      case op: KvpConcreteTypeHead[a, ht, nt, ho, xl, xll] =>
        val headF = kvpHList(op.hListConvert.from)
        val tailF = kvpHList(op.tail)
        import shapeless.::
        path =>
          {
            val rsToHead = headF(path)
            val rsToTail = tailF(path)
            rs =>
              {
                Util.eitherMap2(rsToHead(rs), rsToTail(rs))(
                  (l1: xl, l2: ht) => {
                    op.isHCons.cons(op.hListConvert.fHtoA(l1), l2)
                  })
              }
          }
      case op: KvpHListHead[a, al, h, hl, t, tl] =>
        val headF = kvpHList(op.head)
        val tailF = kvpHList(op.tail)
        path =>
          {
            val rsToHead = headF(path)
            val rsToTail = tailF(path)
            rs =>
              {
                Util.eitherMap2(rsToHead(rs), rsToTail(rs))((l1: h, l2: t) => {
                  op.prepend(l1, l2)
                })
              }
          }
    }

  def valueDefinition[A](fgo: KvpValue[A]): (
      Path,
      FieldName) => ResultSet => Either[NonEmptyList[ExtractionError], A] =
    fgo match {
      case op: OptionalKvpValueDefinition[a] =>
        (path, fieldName) =>
          val child = valueDefinition(op.valueDefinitionOp)(path, fieldName)
          rs => {
            child(rs) match {
              case Left(errs) =>
                if (errs.length == 1) errs.head match {
                  case RequiredData(_, childOp) if childOp == op.valueDefinitionOp => Right(None)
                  case _ => Left(errs)
                } else {
                  Left[NonEmptyList[ExtractionError], Option[a]](errs)
                }
              case Right(a) => Right(Some(a))
            }
          }: Either[NonEmptyList[ExtractionError], Option[a]]
      case ob: BooleanData =>
        (path, fieldName) => rs =>
          catchSql(rs.getBoolean(fieldName), path, ob)
      case sd: StringData =>
        (path, fieldName) => rs =>
          catchSql(rs.getString(fieldName), path, sd)
      case id: ShortData =>
        (path, fieldName) => rs =>
          catchSql(rs.getShort(fieldName), path, id)
      case id: IntData =>
        (path, fieldName) => rs =>
          catchSql(rs.getInt(fieldName), path, id)
      case ri: LongData =>
        (path, fieldName) => rs =>
          catchSql(rs.getLong(fieldName), path, ri)
      case uu: UuidData =>
        (path, fieldName) => rs =>
          catchSql[String](rs.getString(fieldName), path, uu)
            .flatMap(str => stringToUuid(str, path))
      case dd: LocalDateTimeData =>
        (path, fieldName) => rs =>
          catchSql(rs.getDate(fieldName, utcCalendar), path, dd)
            .map(
              date =>
                LocalDateTime.ofInstant(new Date(date.getTime).toInstant,
                                        ZoneId.of("UTC")))
      case ld: LocalDateData =>
        (path, fieldName) => rs =>
          catchSql(rs.getDate(fieldName, utcCalendar), path, ld)
            .map(
              date => date.toLocalDate)
      case fd: FloatData =>
        (path, fieldName) => rs =>
          catchSql(rs.getFloat(fieldName), path, fd)
      case dd: DoubleData =>
        (path, fieldName) => rs =>
          catchSql(rs.getDouble(fieldName), path, dd)
      case bd: BigDecimalData =>
        (path, fieldName) => rs =>
          catchSql(rs.getBigDecimal(fieldName), path, bd).map(bd =>
            BigDecimal(bd))
      case ba: ByteArrayData =>
        (path, fieldName) => rs =>
          catchSql(rs.getBytes(fieldName), path, ba)
      case ld: ListData[t]      => ???
      case ed: EitherData[a, b] =>
        (path, fieldName) => rs => {
          val result = valueDefinition(ed.definitionA)(path, "left_" + fieldName)(rs) match {
            //if the error is that the left is required, we will check the right.
            case Left(nel) =>
              if (nel.length == 1) {
                nel.head match {
                  case RequiredData(path, op) if op == ed.definitionA => {
                    valueDefinition(ed.definitionB)(path, "right_" + fieldName)(rs).map(Right(_))
                  }
                  case _ => Left(nel)
                }
              } else {
                Left(nel)
              }
            case Right(v) => Right(Left(v))
          }
          result
        }

      case esd: EnumerationData[e,a] =>
        (path, fieldName) => rs => {
          for {
            r <- catchSql(rs.getString(fieldName), path, esd)
            e <- stringToEnumeration[e,a](r, path, esd.enumeration)(esd.manifestOfA)
          } yield e.asInstanceOf[A]
        }:Either[NonEmptyList[com.bones.data.Error.ExtractionError], A]
      case kvp: KvpHListValue[h, hl] =>
        val groupF = kvpHList(kvp.kvpHList)
        (path, _) => //Ignore fieldName here
          groupF(path).andThen(_.map(_.asInstanceOf[A]))

      case x: HListConvert[a, al, b] =>
        val groupF = kvpHList(x.from)
        (path, _) =>
          groupF(path).andThen(_.map(x.fHtoA))
    }

  private def catchSql[A](
      f: => A,
      path: Path,
      op: KvpValue[_]): Either[NonEmptyList[ExtractionError], A] =
    try {
      val result = f
      if (result == null) {
        Left(NonEmptyList.one(RequiredData(path, op)))
      } else {
        Right(result)
      }
    } catch {
      case ex: SQLException =>
        Left(NonEmptyList.one(SystemError(path, ex, None)))
    }

}
