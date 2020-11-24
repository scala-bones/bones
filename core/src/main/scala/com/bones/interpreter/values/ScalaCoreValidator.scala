package com.bones.interpreter.values

import java.util.Base64

import com.bones.Path
import com.bones.Util.stringToEnumeration
import com.bones.data.Error.{CanNotConvert, ExtractionErrors, RequiredValue}
import com.bones.data.values._
import com.bones.interpreter.{
  InterchangeFormatPrimitiveValidator,
  InterchangeFormatValidatorValue,
  OptionalInputValidator
}

import scala.util.Try

trait ScalaCoreValidator[IN] extends InterchangeFormatValidatorValue[ScalaCoreValue, IN] {

  val baseValidator: InterchangeFormatPrimitiveValidator[IN]

  override def createValidator[A](
    alg: ScalaCoreValue[A]): OptionalInputValidator[String, ScalaCoreValue, A, IN] = {
    alg match {
      case op: StringData =>
        baseValidator.required(
          alg.typeName,
          op.validations,
          baseValidator.extractString(alg.typeName))
      case id: IntData =>
        baseValidator.required(alg.typeName, id.validations, baseValidator.extractInt)
      case op: LongData =>
        baseValidator.required(alg.typeName, op.validations, baseValidator.extractLong)
      case op: BooleanData =>
        baseValidator.required(alg.typeName, op.validations, baseValidator.extractBool)
      case op @ ByteArrayData(validations) =>
        val decoder = Base64.getDecoder
        (inOpt: Option[IN], path: Path[String]) =>
          {
            val result = for {
              in <- inOpt.toRight[ExtractionErrors[String]](List(RequiredValue(path, alg.typeName)))
              str <- baseValidator.extractString(alg.typeName).validateWithPath(in, path)
              arr <- Try {
                decoder.decode(str)
              }.toEither.left.map(thr =>
                List(CanNotConvert(path, str, classOf[Array[Byte]], Some(thr))))
            } yield arr
            result.asInstanceOf[Either[ExtractionErrors[String], A]]
          }
      case fd: FloatData =>
        baseValidator.required(alg.typeName, fd.validations, baseValidator.extractFloat)
      case dd: DoubleData =>
        baseValidator.required(alg.typeName, dd.validations, baseValidator.extractDouble)
      case sd: ShortData =>
        baseValidator.required(alg.typeName, sd.validations, baseValidator.extractShort)
      case op: BigDecimalData =>
        baseValidator.required(alg.typeName, op.validations, baseValidator.extractBigDecimal)
      case op: EnumerationData[e, A] =>
        (inOpt: Option[IN], path: Path[String]) =>
          for {
            in <- inOpt.toRight[ExtractionErrors[String]](List(RequiredValue(path, alg.typeName)))
            str <- baseValidator.extractString(alg.typeName).validateWithPath(in, path)
            enum <- stringToEnumeration[String, e, A](str, path, op.enumeration.asInstanceOf[e])
          } yield enum.asInstanceOf[A]

    }
  }
}
