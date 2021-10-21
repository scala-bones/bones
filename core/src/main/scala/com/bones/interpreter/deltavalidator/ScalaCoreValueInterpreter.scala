package com.bones.interpreter.deltavalidator

import com.bones.Path
import com.bones.Util.{CanBeOmitted, stringToEnumeration}
import com.bones.data.Error.{CanNotConvert, ExtractionErrors, RequiredValue}
import com.bones.data.values.{
  BigDecimalData,
  BooleanData,
  ByteArrayData,
  DoubleData,
  EnumerationData,
  FloatData,
  IntData,
  LongData,
  ScalaCoreValue,
  ShortData,
  StringData
}
import com.bones.validation.ValidationUtil

import java.util.Base64
import scala.util.Try

trait ScalaCoreValueInterprete[IN]
    extends InterchangeFormatDeltaValidatorValue[ScalaCoreValue, IN] {
  val primitive: PrimitiveInterchangeFormat[IN, String]
  val decoder = Base64.getDecoder

  override def createDeltaValidator[A](
    alg: ScalaCoreValue[A]
  ): DeltaValueValidator[String, ScalaCoreValue, A, IN] = {
    val baseValidator: DeltaValueValidator[String, ScalaCoreValue, A, IN] = alg match {
      case op: StringData => primitive.extractString(alg.typeName)
      case id: IntData =>
        primitive.extractInt[ScalaCoreValue]
      case op: LongData =>
        primitive.extractLong[ScalaCoreValue]
      case op: BooleanData =>
        primitive.extractBool[ScalaCoreValue]
      case op: ByteArrayData =>
        primitive
          .extractString[ScalaCoreValue](alg.typeName)
          .flatMapA(str =>
            new DeltaValueValidator[String, ScalaCoreValue, Array[Byte], IN] {
              override def extract(in: IN, key: String, path: List[String])
                : Either[ExtractionErrors[String], CanBeOmitted[String, Array[Byte]]] =
                Try {
                  decoder.decode(str)
                }.toEither.left
                  .map(thr => List(CanNotConvert(path, str, classOf[Array[Byte]], Some(thr))))
                  .map(Right(_))
            }
          )

      case fd: FloatData =>
        primitive.extractFloat[ScalaCoreValue]
      case dd: DoubleData =>
        primitive.extractDouble[ScalaCoreValue]
      case sd: ShortData =>
        primitive.extractShort[ScalaCoreValue]
      case op: BigDecimalData =>
        primitive.extractBigDecimal[ScalaCoreValue]
      case op: EnumerationData[e, A] =>
        primitive
          .extractString(alg.typeName)
          .flatMapA(str =>
            new DeltaValueValidator[String, ScalaCoreValue, A, IN] {
              override def extract(in: IN, key: String, path: List[String])
                : Either[ExtractionErrors[String], CanBeOmitted[String, A]] =
                stringToEnumeration[String, e, A](str, path, op.enumeration)
                  .map(x => Right(x.asInstanceOf[A]))
            }
          )

    }
    baseValidator
      .asInstanceOf[DeltaValueValidator[String, ScalaCoreValue, A, IN]]
      .addValidation(ValidationUtil.validate(alg.validations))

  }
}
