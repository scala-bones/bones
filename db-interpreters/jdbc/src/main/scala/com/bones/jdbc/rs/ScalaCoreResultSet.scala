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
          catchSql(rs.getBoolean(fieldName), alg.typeName, path, ob)
      case sd: StringData =>
        (path, fieldName) => rs =>
          catchSql(rs.getString(fieldName), alg.typeName, path, sd)
      case id: ShortData =>
        (path, fieldName) => rs =>
          catchSql(rs.getShort(fieldName), alg.typeName, path, id)
      case id: IntData =>
        (path, fieldName) => rs =>
          catchSql(rs.getInt(fieldName), alg.typeName, path, id)
      case ri: LongData =>
        (path, fieldName) => rs =>
          catchSql(rs.getLong(fieldName), alg.typeName, path, ri)
      case fd: FloatData =>
        (path, fieldName) => rs =>
          catchSql(rs.getFloat(fieldName), alg.typeName, path, fd)
      case dd: DoubleData =>
        (path, fieldName) => rs =>
          catchSql(rs.getDouble(fieldName), alg.typeName, path, dd)
      case bd: BigDecimalData =>
        (path, fieldName) => rs =>
          catchSql(rs.getBigDecimal(fieldName), alg.typeName, path, bd).map(bd => BigDecimal(bd))
      case ba: ByteArrayData =>
        (path, fieldName) => rs =>
          catchSql(rs.getBytes(fieldName), alg.typeName, path, ba)
      case esd: EnumerationData[e, a] =>
        (path, fieldName) => rs =>
          {
            for {
              r <- catchSql(rs.getString(fieldName), alg.typeName, path, esd)
              e <- stringToEnumeration[e, a](r, path, esd.enumeration)
            } yield e.asInstanceOf[A]
          }: Either[NonEmptyList[com.bones.data.Error.ExtractionError], A]

    }
}
