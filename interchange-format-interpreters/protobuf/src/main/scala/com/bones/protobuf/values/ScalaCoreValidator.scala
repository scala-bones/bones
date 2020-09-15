package com.bones.protobuf.values

import com.bones.Util.{stringToBigDecimal, stringToEnumeration}
import com.bones.data.values._
import com.bones.protobuf.ProtobufSequentialValidatorInterpreter._
import com.bones.protobuf.{ExtractFromProto, ProtobufValidatorValue}

trait ScalaCoreValidator extends ProtobufValidatorValue[ScalaCoreValue] {
  override def extractFromProto[A](alg: ScalaCoreValue[A]): ExtractFromProto[A] =
    alg match {
      case bd: BooleanData   => booleanData(bd.validations)
      case rs: StringData    => stringData(rs.validations)
      case sd: ShortData     => shortData(sd.validations)
      case id: IntData       => intData(id.validations)
      case ld: LongData      => longData(ld.validations)
      case ba: ByteArrayData => byteArrayData(ba.validations)
      case fd: FloatData     => floatData(fd.validations)
      case dd: DoubleData    => doubleData(dd.validations)
      case bd: BigDecimalData =>
        stringDataWithFlatMap(bd.typeName, stringToBigDecimal, bd.validations)
      case esd: EnumerationData[e, a] =>
        stringDataWithFlatMap(esd.typeName, (str, path) => {
          stringToEnumeration(str, path, esd.enumeration)
            .map(_.asInstanceOf[A])
        }, esd.validations)

    }
}
