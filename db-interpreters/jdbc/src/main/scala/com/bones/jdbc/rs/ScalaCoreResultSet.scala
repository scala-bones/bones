package com.bones.jdbc.rs

import java.sql.ResultSet

import com.bones.Path
import com.bones.Util.{NullableResult, stringToEnumeration}
import com.bones.data.Error.ExtractionErrors
import com.bones.data.values._
import com.bones.jdbc.FindInterpreter.FieldName

trait ScalaCoreResultSet extends ResultSetValue[ScalaCoreValue] {
  override def resultSet[A](alg: ScalaCoreValue[A]): (
    Path[String],
    FieldName) => ResultSet => Either[ExtractionErrors[String], NullableResult[String, A]] =
    alg match {
      case ob: BooleanData =>
        (path, fieldName) => rs =>
          catchSql(rs.getBoolean(fieldName), alg.typeName, fieldName, path)
      case sd: StringData =>
        (path, fieldName) => rs =>
          catchSql(rs.getString(fieldName), alg.typeName, fieldName, path)
      case id: ShortData =>
        (path, fieldName) => rs =>
          catchSql(rs.getShort(fieldName), alg.typeName, fieldName, path)
      case id: IntData =>
        (path, fieldName) => rs =>
          catchSql(rs.getInt(fieldName), alg.typeName, fieldName, path)
      case ri: LongData =>
        (path, fieldName) => rs =>
          catchSql(rs.getLong(fieldName), alg.typeName, fieldName, path)
      case fd: FloatData =>
        (path, fieldName) => rs =>
          catchSql(rs.getFloat(fieldName), alg.typeName, fieldName, path)
      case dd: DoubleData =>
        (path, fieldName) => rs =>
          catchSql(rs.getDouble(fieldName), alg.typeName, fieldName, path)
      case bd: BigDecimalData =>
        (path, fieldName) => rs =>
          catchSql(rs.getBigDecimal(fieldName), alg.typeName, fieldName, path)
      case ba: ByteArrayData =>
        (path, fieldName) => rs =>
          catchSql(rs.getBytes(fieldName), alg.typeName, fieldName, path)
      case esd: EnumerationData[e, a] =>
        (path, fieldName) => rs =>
          {
            for {
              r <- catchSql(rs.getString(fieldName), alg.typeName, fieldName, path)
              e <- {
                r match {
                  case Left(nv) => Right(Left(nv))
                  case Right(str) =>
                    stringToEnumeration[String, e, a](str, path, esd.enumeration).map(v =>
                      Right(v.asInstanceOf[A]))
                }
              }
            } yield e
          }: Either[ExtractionErrors[String], NullableResult[String, A]]

    }
}
