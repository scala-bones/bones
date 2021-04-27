package com.bones.akkahttp.server.search

import akka.http.scaladsl.unmarshalling.{FromStringUnmarshaller, Unmarshaller}
import akka.stream.Materializer
import com.bones.Util
import com.bones.data.values.{
  BaseScalaCoreInterpreter,
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

import scala.concurrent.ExecutionContext

trait FromStringUnmarshallerInterpreter[ALG[_]] {
  def getFromStringUnmarshaller[A](alg: ALG[A]): FromStringUnmarshaller[A]
}

class ScalaCoreFromStringUnmarshallerInterpreter(
  implicit ec: ExecutionContext,
  materializer: Materializer)
    extends FromStringUnmarshallerInterpreter[ScalaCoreValue] {
  override def getFromStringUnmarshaller[A](alg: ScalaCoreValue[A]): Unmarshaller[String, A] = {
    val result = alg match {
      case bd: BooleanData    => Unmarshaller.booleanFromStringUnmarshaller
      case id: IntData        => Unmarshaller.intFromStringUnmarshaller
      case ld: LongData       => Unmarshaller.longFromStringUnmarshaller
      case sd: ShortData      => Unmarshaller.shortFromStringUnmarshaller
      case sd: StringData     => Unmarshaller.stringUnmarshaller
      case fd: FloatData      => Unmarshaller.floatFromStringUnmarshaller
      case dd: DoubleData     => Unmarshaller.doubleFromStringUnmarshaller
      case bd: BigDecimalData => numberUnmarshaller(BigDecimal(_), "BigDecimal")
      case ba: ByteArrayData  => Unmarshaller.byteArrayUnmarshaller
      case en: EnumerationData[e, v] =>
        Unmarshaller.stringUnmarshaller.map(str => {
          Util.stringToEnumeration[String, e, v](str, List.empty, en.enumeration) match {
            case Left(_) =>
              throw new IllegalArgumentException(s"could not convert $str into Enum ${en.typeName}")
            case Right(value) => value
          }
        })
    }
    result.asInstanceOf[Unmarshaller[String, A]]
  }

  /** Copied from AkkaHttp because it was private.*/
  private def numberUnmarshaller[T](f: String => T, target: String): Unmarshaller[String, T] =
    Unmarshaller.strict[String, T] { string =>
      try f(string)
      catch numberFormatError(string, target)
    }

  /** Copied from AkkaHttp because it was private.*/
  private def numberFormatError(
    value: String,
    target: String): PartialFunction[Throwable, Nothing] = {
    case e: NumberFormatException =>
      throw if (value.isEmpty) Unmarshaller.NoContentException
      else new IllegalArgumentException(s"'$value' is not a valid $target value", e)
  }
}
