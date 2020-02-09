package com.bones.interpreter.custom

import com.bones.data.Error.CanNotConvert
import com.bones.data.custom.{CanNotConvertData, ExtractionErrorValue}
import com.bones.interpreter.KvpInterchangeFormatEncoderInterpreter
import com.bones.interpreter.KvpInterchangeFormatEncoderInterpreter.{InterchangeFormatEncoder, NoAlgebraEncoder}
import com.bones.syntax._
import shapeless.{HNil, ::}
import shapeless.syntax.std.tuple._

object ExtractionErrorEncoder {

  def canNotConvertToHList(canNotConvert: CanNotConvert[_,_]): String :: String :: String :: Option[String] :: HNil = {
    val path = canNotConvert.path.mkString(".")
    val input = canNotConvert.input.toString
    val expectedType = canNotConvert.toType.getCanonicalName
    val stack = canNotConvert.cause.map(_.getStackTrace.toSeq.map(_.toString).mkString("\n"))
    path :: input :: expectedType :: stack :: HNil
  }


  val canNotConvertSchema =
    (
      ("path", string) :<:
      ("input", string) :<:
      ("expectedType", string) :<:
      ("stackTrace", string.optional) :<:
      kvpNil
    ).xmap[CanNotConvert[_,_]](h => sys.error("Mapping to an ExtractionError is not supported"), canNotConvertToHList)

}

trait ExtractionErrorEncoder[OUT] extends InterchangeFormatEncoder[ExtractionErrorValue, OUT] {

  import ExtractionErrorEncoder._

  val defaultEncoder: KvpInterchangeFormatEncoderInterpreter[OUT]

  override def encode[A](alg: ExtractionErrorValue[A]): A => OUT =
    alg match {
      case CanNotConvertData =>
        (canNotConvert: CanNotConvert[_,_]) => {
          val encoder = defaultEncoder
            .encoderFromCustomSchema[NoAlgebra, CanNotConvert[_,_]](canNotConvertSchema, NoAlgebraEncoder[OUT])
          encoder(canNotConvert)
        }
    }


}
