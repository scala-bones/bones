package com.bones.jdbc.rs

import java.sql.ResultSet

import cats.data.NonEmptyList
import com.bones.Util.stringToEnumeration
import com.bones.data.Error
import com.bones.data.values._
import com.bones.jdbc.FindInterpreter.{FieldName, Path}

trait ScalaCoreResultSet extends ResultSetValue[ScalaCoreValue] {
  override def resultSet[A](alg: ScalaCoreValue[A])
    : (Path, FieldName) => ResultSet => Either[NonEmptyList[Error.ExtractionError], A] =
    alg match {
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
      case fd: FloatData =>
        (path, fieldName) => rs =>
          catchSql(rs.getFloat(fieldName), path, fd)
      case dd: DoubleData =>
        (path, fieldName) => rs =>
          catchSql(rs.getDouble(fieldName), path, dd)
      case bd: BigDecimalData =>
        (path, fieldName) => rs =>
          catchSql(rs.getBigDecimal(fieldName), path, bd).map(bd => BigDecimal(bd))
      case ba: ByteArrayData =>
        (path, fieldName) => rs =>
          catchSql(rs.getBytes(fieldName), path, ba)
      case esd: EnumerationData[e, a] =>
        (path, fieldName) => rs =>
          {
            for {
              r <- catchSql(rs.getString(fieldName), path, esd)
              e <- stringToEnumeration[e, a](r, path, esd.enumeration)(esd.manifestOfA)
            } yield e.asInstanceOf[A]
          }: Either[NonEmptyList[com.bones.data.Error.ExtractionError], A]

    }
}
