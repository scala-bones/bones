package com.bones.protobuf.custom

import com.bones.Util.{stringToBigDecimal, stringToEnumeration}
import com.bones.data.custom._
import com.bones.protobuf.ProtobufSequentialValidatorInterpreter._

trait ScalaCoreValidatorInterpreter extends CustomValidatorInterpreter[ScalaCoreValue] {
  override def extractFromProto[A](alg: ScalaCoreValue[A]): ExtractFromProto[A] =
    alg match {
      case bd: BooleanData   => booleanData(bd, bd.validations)
      case rs: StringData    => stringData(rs, rs.validations)
      case sd: ShortData     => shortData(sd, sd.validations)
      case id: IntData       => intData(id, id.validations)
      case ld: LongData      => longData(ld, ld.validations)
      case ba: ByteArrayData => byteArrayData(ba, ba.validations)
      case fd: FloatData     => floatData(fd, fd.validations)
      case dd: DoubleData    => doubleData(dd, dd.validations)
      case bd: BigDecimalData =>
        stringDataWithFlatMap(Left(bd), stringToBigDecimal, bd.validations)
      case esd: EnumerationData[e, a] =>
        stringDataWithFlatMap(Left(esd), (str, path) => {
          stringToEnumeration(str, path, esd.enumeration)(esd.manifestOfA)
            .map(_.asInstanceOf[A])
        }, esd.validations)

    }
}
