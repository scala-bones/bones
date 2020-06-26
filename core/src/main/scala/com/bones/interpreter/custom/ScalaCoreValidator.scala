package com.bones.interpreter.custom

import java.util.Base64

import cats.data.NonEmptyList
import com.bones.Path
import com.bones.Util.stringToEnumeration
import com.bones.data.Error
import com.bones.data.Error.{CanNotConvert, ExtractionError, RequiredValue}
import com.bones.data.custom._
import com.bones.interpreter.KvpInterchangeFormatValidatorInterpreter
import com.bones.interpreter.KvpInterchangeFormatValidatorInterpreter.InterchangeFormatValidator

import scala.util.Try

trait ScalaCoreValidator[IN] extends InterchangeFormatValidator[ScalaCoreValue, IN] {
  
  val baseValidator: KvpInterchangeFormatValidatorInterpreter[IN]
  
  override def validate[A](alg: ScalaCoreValue[A]): (Option[IN], List[String]) => Either[NonEmptyList[Error.ExtractionError], A] = {
    alg match {
      case op: StringData =>
        baseValidator.required(Right(op), op.validations, baseValidator.extractString(Right(op), classOf[String]))
      case id: IntData =>
        baseValidator.required(Right(id), id.validations, baseValidator.extractInt(Right(id)))
      case op: LongData =>
        baseValidator.required(Right(op), op.validations, baseValidator.extractLong(Right(op)))
      case op: BooleanData =>
        baseValidator.required(Right(op), op.validations, baseValidator.extractBool(Right(op)))
      case op @ ByteArrayData(validations) =>
        val decoder = Base64.getDecoder
        (inOpt: Option[IN], path: Path) =>
          for {
            in <- inOpt.toRight[NonEmptyList[ExtractionError]](
              NonEmptyList.one(RequiredValue(path, Right(op))))
            str <- baseValidator.extractString(Right(op), classOf[Array[Byte]])(in, path)
            arr <- Try {
              decoder.decode(str)
            }.toEither.left.map(thr =>
              NonEmptyList.one(CanNotConvert(path, str, classOf[Array[Byte]], Some(thr))))
          } yield arr
      case fd: FloatData =>
        baseValidator.required(Right(fd), fd.validations, baseValidator.extractFloat(Left(alg)))
      case dd: DoubleData =>
        baseValidator.required(Right(dd), dd.validations, baseValidator.extractDouble(Left(alg)))
      case sd: ShortData =>
        baseValidator.required(Right(sd), sd.validations, baseValidator.extractShort(Left(alg)))
      case op: BigDecimalData =>
        baseValidator.required(Right(op), op.validations, baseValidator.extractBigDecimal(Left(alg)))
      case op: EnumerationData[e, A] =>
        (inOpt: Option[IN], path: Path) =>
          for {
            in <- inOpt.toRight[NonEmptyList[ExtractionError]](
              NonEmptyList.one(RequiredValue(path, Right(op))))
            str <- baseValidator.extractString(Right(alg), op.manifestOfA.runtimeClass)(in, path)
            enum <- stringToEnumeration[e, A](str, path, op.enumeration.asInstanceOf[e])(
              op.manifestOfA)
          } yield enum.asInstanceOf[A]



    }
  }
}
