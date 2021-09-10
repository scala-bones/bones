package com.bones.json4s.impl

import com.bones.data.Error.{CanNotConvert, ExtractionErrors, RequiredValue, WrongTypeError}
import com.bones.interpreter.validator.{InterchangeFormatPrimitiveValidator, Validator}
import org.json4s.JsonAST._
import org.json4s.{JInt, JNull, JObject}

import scala.util.Try

object Json4sPrimitiveValidator extends InterchangeFormatPrimitiveValidator[JValue] {

  override def extractString[ALG2[_], A](
    typeName: String
  ): Validator[String, ALG2, String, JValue] =
    (in: JValue, path: List[String]) =>
      in match {
        case JString(value) => Right(value)
        case _              => Left(determineError(in, typeName, path))
      }

  def tryNumberConversion[N, NN: Manifest](
    path: List[String],
    value: N,
    f: N => NN
  ): Either[ExtractionErrors[String], NN] =
    Try(f(value)).toEither.left.map(ex =>
      List(CanNotConvert(path, value, manifest[NN].runtimeClass, Some(ex)))
    )

  override def extractInt[ALG2[_], A]: Validator[String, ALG2, Int, JValue] = {
    (in: JValue, path: List[String]) =>
      in match {
        case JInt(i)     => Right(i.toInt)
        case JLong(l)    => tryNumberConversion[Long, Int](path, l, _.toInt)
        case JDecimal(d) => tryNumberConversion[BigDecimal, Int](path, d, _.toInt)
        case JDouble(d)  => tryNumberConversion[Double, Int](path, d, _.toInt)
        case _           => Left(List(WrongTypeError(path, "Int", in.getClass.getSimpleName, None)))
      }
  }

  override def extractFloat[ALG2[_], A]: Validator[String, ALG2, Float, JValue] = {
    (in: JValue, path: List[String]) =>
      in match {
        case JInt(i)     => tryNumberConversion[BigInt, Float](path, i, _.toFloat)
        case JLong(l)    => tryNumberConversion[Long, Float](path, l, _.toFloat)
        case JDecimal(d) => tryNumberConversion[BigDecimal, Float](path, d, _.toFloat)
        case JDouble(d)  => tryNumberConversion[Double, Float](path, d, _.toFloat)
        case _ =>
          Left(List(WrongTypeError(path, "Float", in.getClass.getSimpleName, None)))
      }
  }

  override def extractDouble[ALG2[_], A]: Validator[String, ALG2, Double, JValue] = {
    (in: JValue, path: List[String]) =>
      in match {
        case JInt(i)     => tryNumberConversion[BigInt, Double](path, i, _.toDouble)
        case JLong(l)    => tryNumberConversion[Long, Double](path, l, _.toDouble)
        case JDecimal(d) => tryNumberConversion[BigDecimal, Double](path, d, _.toDouble)
        case JDouble(d)  => Right(d)
        case _ =>
          Left(List(WrongTypeError(path, "Double", in.getClass.getSimpleName, None)))
      }
  }

  override def extractLong[ALG2[_], A]: Validator[String, ALG2, Long, JValue] = {
    (in: JValue, path: List[String]) =>
      {
        in match {
          case JInt(i)     => tryNumberConversion[BigInt, Long](path, i, _.toLong)
          case JLong(l)    => Right(l)
          case JDecimal(d) => tryNumberConversion[BigDecimal, Long](path, d, _.toLong)
          case JDouble(d)  => tryNumberConversion[Double, Long](path, d, _.toLong)
          case _ =>
            Left(List(WrongTypeError(path, "Long", in.getClass.getSimpleName, None)))
        }
      }
  }

  override def extractShort[ALG2[_], A]: Validator[String, ALG2, Short, JValue] = {
    (in: JValue, path: List[String]) =>
      in match {
        case JInt(i)     => tryNumberConversion[BigInt, Short](path, i, _.toShort)
        case JLong(l)    => tryNumberConversion[Long, Short](path, l, _.toShort)
        case JDecimal(d) => tryNumberConversion[BigDecimal, Short](path, d, _.toShort)
        case JDouble(d)  => tryNumberConversion[Double, Short](path, d, _.toShort)
        case _ =>
          Left(List(WrongTypeError(path, "Short", in.getClass.getSimpleName, None)))
      }
  }

  override def extractBool[ALG2[_], A]: Validator[String, ALG2, Boolean, JValue] = {
    (in: JValue, path: List[String]) =>
      in match {
        case JBool(bool) => Right(bool)
        case _           => Left(determineError(in, "Boolean", path))
      }
  }

  override def extractArray[ALG2[_], A](
    typeName: String
  ): Validator[String, ALG2, Seq[JValue], JValue] = { (in: JValue, path: List[String]) =>
    in match {
      case JArray(elements) => Right(elements)
      case _                => Left(determineError(in, typeName, path))
    }
  }

  override def extractBigDecimal[ALG2[_], A]: Validator[String, ALG2, BigDecimal, JValue] = {
    (in: JValue, path: List[String]) =>
      in match {
        case JInt(i)     => tryNumberConversion[BigInt, BigDecimal](path, i, BigDecimal(_))
        case JLong(l)    => tryNumberConversion[Long, BigDecimal](path, l, BigDecimal(_))
        case JDecimal(d) => Right(d)
        case JDouble(d)  => tryNumberConversion[Double, BigDecimal](path, d, BigDecimal(_))
        case _ =>
          Left(List(WrongTypeError(path, "BigDecimal", in.getClass.getSimpleName, None)))
      }
  }

  override def stringValue(in: JValue, elementName: String): Option[String] = {
    val item = for {
      JObject(child) <- in
      JField(name, JString(elementValue)) <- child
      if name == elementName
    } yield elementValue
    item.headOption
  }

  protected def determineError[ALG2[_], A](
    in: JValue,
    typeName: String,
    path: List[String]
  ): ExtractionErrors[String] = {
    val error = in match {
      case JNull => RequiredValue(path, typeName)
      case _     => WrongTypeError(path, typeName, in.getClass.getSimpleName, None)
    }
    List(error)
  }

}
