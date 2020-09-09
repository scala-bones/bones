package com.bones.jdbc.update

import java.sql.Types

import com.bones.data.values._

trait ScalaCoreUpdateStatement extends UpdateStatementValue[ScalaCoreValue] {

  override def definitionResult[A](alg: ScalaCoreValue[A]): (Index, Key) => JdbcColumnStatement[A] =
    alg match {
      case ob: BooleanData =>
        psF(i => (ps, a) => ps.setBoolean(i, a), Types.BOOLEAN)
      case rs: StringData =>
        psF(i => (ps, a) => ps.setString(i, a), Types.LONGVARCHAR)
      case ri: IntData    => psF(i => (ps, a) => ps.setInt(i, a), Types.INTEGER)
      case ri: ShortData  => psF(i => (ps, a) => ps.setShort(i, a), Types.SMALLINT)
      case ri: LongData   => psF(i => (ps, a) => ps.setLong(i, a), Types.BIGINT)
      case fd: FloatData  => psF(i => (ps, a) => ps.setFloat(i, a), Types.FLOAT)
      case fd: DoubleData => psF(i => (ps, a) => ps.setDouble(i, a), Types.DOUBLE)
      case bd: BigDecimalData =>
        psF[BigDecimal](i => (ps, a) => ps.setBigDecimal(i, a.underlying), Types.NUMERIC)
      case ba: ByteArrayData =>
        psF[Array[Byte]](i => (ps, a) => ps.setBytes(i, a), Types.BINARY)
      case esd: EnumerationData[e, a] =>
        psF(i => (ps, a) => ps.setString(i, a.toString), Types.VARCHAR)

    }
}
