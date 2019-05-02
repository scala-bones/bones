package com.bones.jdbc

import java.sql.{ResultSet, SQLException}
import java.time.{ZoneId, ZonedDateTime}
import java.util.Date

import cats.data.NonEmptyList
import com.bones.Util
import com.bones.Util.{stringToEnum, stringToEnumeration, stringToUuid}
import com.bones.data.Error.{ExtractionError, RequiredData, SystemError}
import com.bones.data.Value._
import DbUtil.camelToSnake
import FindInterpreter.{FieldName, Path, utcCalendar}
import shapeless.{HList, HNil, Nat}

/** Responsible for converting a result set into the result type */
object ResultSetInterpreter {

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
                  (l1 :: l2).asInstanceOf[a]
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
                    (op.hListConvert.fab(l1) :: l2).asInstanceOf[H]
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

  def valueDefinition[A](fgo: ValueDefinitionOp[A]): (
      Path,
      FieldName) => ResultSet => Either[NonEmptyList[ExtractionError], A] =
    fgo match {
      case op: OptionalValueDefinition[a] =>
        (path, fieldName) =>
          val child = valueDefinition(op.valueDefinitionOp)(path, fieldName)
          rs =>
            val result = child(rs) match {
              case Left(i) =>
                if (i.length == 1) i.head match {
                  case RequiredData(_, childOp)
                      if childOp == op.valueDefinitionOp =>
                    Right(None)
                  case _ => Left(i)
                } else {
                  Left(i)
                }
              case x => x.map(s => Some(s))
            }
            result
              .asInstanceOf[Either[NonEmptyList[ExtractionError], Option[a]]]
      case ob: BooleanData =>
        (path, fieldName) => rs =>
          catchSql(rs.getBoolean(fieldName), path, ob)
      case sd: StringData =>
        (path, fieldName) => rs =>
          catchSql(rs.getString(fieldName), path, sd)
      case ri: LongData =>
        (path, fieldName) => rs =>
          catchSql(rs.getLong(fieldName), path, ri)
      case uu: UuidData =>
        (path, fieldName) => rs =>
          catchSql[String](rs.getString(fieldName), path, uu)
            .flatMap(str => stringToUuid(str, path))
      case dd: DateTimeData =>
        (path, fieldName) => rs =>
          catchSql(rs.getDate(fieldName, utcCalendar), path, dd)
            .map(
              date =>
                ZonedDateTime.ofInstant(new Date(date.getTime).toInstant,
                                        ZoneId.of("UTC")))
      case bd: BigDecimalData =>
        (path, fieldName) => rs =>
          catchSql(rs.getBigDecimal(fieldName), path, bd).map(bd =>
            BigDecimal(bd))
      case ba: ByteArrayData =>
        (path, fieldName) => rs =>
          catchSql(rs.getBytes(fieldName), path, ba)
      case ld: ListData[t]      => ???
      case ed: EitherData[a, b] => ???
      case esd: EnumerationStringData[a] =>
        (path, fieldName) => rs =>
          val result = for {
            r <- catchSql(rs.getString(fieldName), path, esd)
            e <- stringToEnumeration(r, path, esd.enumeration, esd.manifestOfA)
          } yield e
          result.asInstanceOf[
            Either[NonEmptyList[com.bones.data.Error.ExtractionError], A]]
      case kvp: KvpHListValue[h, hl] =>
        val groupF = kvpHList(kvp.kvpHList)
        (path, _) => //Ignore fieldName here
          groupF(path).andThen(_.map(_.asInstanceOf[A]))

      case x: HListConvert[a, al, b] =>
        val groupF = kvpHList(x.from)
        (path, _) =>
          groupF(path).andThen(_.map(x.fab))
      case s: SumTypeData[a, b] =>
        val fromF = valueDefinition(s.from)
        (path, fieldName) =>
          fromF(path, fieldName).andThen(_.flatMap(a =>
            s.fab(a, path).left.map(NonEmptyList.one(_))))
    }

  private def catchSql[A](
      f: => A,
      path: Path,
      op: ValueDefinitionOp[_]): Either[NonEmptyList[ExtractionError], A] =
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
