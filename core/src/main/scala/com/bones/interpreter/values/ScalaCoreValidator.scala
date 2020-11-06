package com.bones.interpreter.values

import java.util.Base64

import cats.data.NonEmptyList
import com.bones.Path
import com.bones.Util.stringToEnumeration
import com.bones.data.Error
import com.bones.data.Error.{CanNotConvert, ExtractionError, ExtractionErrors, RequiredValue}
import com.bones.data.values._
import com.bones.interpreter.{
  InterchangeFormatPrimitiveValidator,
  InterchangeFormatValidatorValue,
  KvpInterchangeFormatValidatorInterpreter
}

import scala.util.Try

trait ScalaCoreValidator[IN] extends InterchangeFormatValidatorValue[ScalaCoreValue, IN] {

  val baseValidator: InterchangeFormatPrimitiveValidator[IN]

  override def validate[A](
    alg: ScalaCoreValue[A]): (Option[IN], List[String]) => Either[ExtractionErrors[String], A] = {
    alg match {
      case op: StringData =>
        baseValidator.required(
          Right(op),
          alg.typeName,
          op.validations,
          baseValidator.extractString(Right(op), alg.typeName))
      case id: IntData =>
        baseValidator.required(
          Right(id),
          alg.typeName,
          id.validations,
          baseValidator.extractInt(Right(id)))
      case op: LongData =>
        baseValidator.required(
          Right(op),
          alg.typeName,
          op.validations,
          baseValidator.extractLong(Right(op)))
      case op: BooleanData =>
        baseValidator.required(
          Right(op),
          alg.typeName,
          op.validations,
          baseValidator.extractBool(Right(op)))
      case op @ ByteArrayData(validations) =>
        val decoder = Base64.getDecoder
        (inOpt: Option[IN], path: Path[String]) =>
          for {
            in <- inOpt.toRight[ExtractionErrors[String]](
              NonEmptyList.one(RequiredValue(path, alg.typeName)))
            str <- baseValidator.extractString(Right(op), alg.typeName)(in, path)
            arr <- Try {
              decoder.decode(str)
            }.toEither.left.map(thr =>
              NonEmptyList.one(CanNotConvert(path, str, classOf[Array[Byte]], Some(thr))))
          } yield arr
      case fd: FloatData =>
        baseValidator.required(
          Right(fd),
          alg.typeName,
          fd.validations,
          baseValidator.extractFloat(Left(alg)))
      case dd: DoubleData =>
        baseValidator.required(
          Right(dd),
          alg.typeName,
          dd.validations,
          baseValidator.extractDouble(alg))
      case sd: ShortData =>
        baseValidator.required(
          Right(sd),
          alg.typeName,
          sd.validations,
          baseValidator.extractShort(alg))
      case op: BigDecimalData =>
        baseValidator.required(
          Right(op),
          alg.typeName,
          op.validations,
          baseValidator.extractBigDecimal(alg))
      case op: EnumerationData[e, A] =>
        (inOpt: Option[IN], path: Path[String]) =>
          for {
            in <- inOpt.toRight[ExtractionErrors[String]](
              NonEmptyList.one(RequiredValue(path, alg.typeName)))
            str <- baseValidator.extractString(Right(alg), alg.typeName)(in, path)
            enum <- stringToEnumeration[String, e, A](str, path, op.enumeration.asInstanceOf[e])
          } yield enum.asInstanceOf[A]

    }
  }
}
