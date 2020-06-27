package com.bones.jdbc.insert

import com.bones.data.values._
import com.bones.jdbc.insert.DbInsertValues.{InsertPair, psF}

trait ScalaCoreDbInsert extends CustomInterpreter[ScalaCoreValue] {
  override def insertPair[A](alg: ScalaCoreValue[A]): InsertPair[A] =
    alg match {
      case ob: BooleanData =>
        psF[Boolean]((ps, i, a) => ps.setBoolean(i, a))
      case rs: StringData =>
        psF[String]((ps, i, a) => ps.setString(i, a))
      case id: ShortData =>
        psF[Short]((ps, i, a) => ps.setShort(i, a))
      case id: IntData =>
        psF[Int]((ps, i, a) => ps.setInt(i, a))
      case ri: LongData =>
        psF[Long]((ps, i, a) => ps.setLong(i, a))
      case bd: BigDecimalData =>
        psF[BigDecimal]((ps, i, a) => ps.setBigDecimal(i, a.underlying))
      case fd: FloatData =>
        psF[Float]((ps, i, a) => ps.setFloat(i, a))
      case dd: DoubleData =>
        psF[Double]((ps, i, a) => ps.setDouble(i, a))
      case ba: ByteArrayData =>
        psF[scala.Array[Byte]]((ps, i, a) => ps.setBytes(i, a))
      case esd: EnumerationData[e, a] =>
        psF[A]((ps, i, a) => ps.setString(i, a.toString))

    }
}
