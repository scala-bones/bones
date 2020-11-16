package com.bones.json4s.impl

import com.bones.Path
import com.bones.data.Error.{CanNotConvert, ExtractionErrors, RequiredValue, WrongTypeError}
import com.bones.data.ListData
import com.bones.interpreter.InterchangeFormatPrimitiveValidator
import org.json4s.JsonAST._
import org.json4s.{JInt, JNull, JObject}

import scala.util.Try

object Json4sPrimitiveValidator extends InterchangeFormatPrimitiveValidator[JValue] {

  override def extractString[ALG2[_], A](
    op: ALG2[A],
    typeName: String
  )(in: JValue, path: List[String]): Either[ExtractionErrors[String], String] =
    in match {
      case JString(value) => Right(value)
      case _              => Left(determineError(in, op, typeName, path))
    }

  def tryNumberConversion[N, NN: Manifest](
    path: List[String],
    value: N,
    f: N => NN): Either[ExtractionErrors[String], NN] =
    Try(f(value)).toEither.left.map(ex =>
      List(CanNotConvert(path, value, manifest[NN].runtimeClass, Some(ex))))

  override def extractInt[ALG2[_], A](
    op: ALG2[A]
  )(in: JValue, path: List[String]): Either[ExtractionErrors[String], Int] = {
    in match {
      case JInt(i)     => Right(i.toInt)
      case JLong(l)    => tryNumberConversion[Long, Int](path, l, _.toInt)
      case JDecimal(d) => tryNumberConversion[BigDecimal, Int](path, d, _.toInt)
      case JDouble(d)  => tryNumberConversion[Double, Int](path, d, _.toInt)
      case _           => Left(List(WrongTypeError(path, "Int", in.getClass.getSimpleName, None)))
    }
  }

  override def extractFloat[ALG2[_], A](
    op: ALG2[A]
  )(in: JValue, path: List[String]): Either[ExtractionErrors[String], Float] = {
    in match {
      case JInt(i)     => tryNumberConversion[BigInt, Float](path, i, _.toFloat)
      case JLong(l)    => tryNumberConversion[Long, Float](path, l, _.toFloat)
      case JDecimal(d) => tryNumberConversion[BigDecimal, Float](path, d, _.toFloat)
      case JDouble(d)  => tryNumberConversion[Double, Float](path, d, _.toFloat)
      case _ =>
        Left(List(WrongTypeError(path, "Float", in.getClass.getSimpleName, None)))
    }
  }

  override def extractDouble[ALG2[_], A](
    op: ALG2[A]
  )(in: JValue, path: List[String]): Either[ExtractionErrors[String], Double] = {
    in match {
      case JInt(i)     => tryNumberConversion[BigInt, Double](path, i, _.toDouble)
      case JLong(l)    => tryNumberConversion[Long, Double](path, l, _.toDouble)
      case JDecimal(d) => tryNumberConversion[BigDecimal, Double](path, d, _.toDouble)
      case JDouble(d)  => Right(d)
      case _ =>
        Left(List(WrongTypeError(path, "Double", in.getClass.getSimpleName, None)))
    }
  }

  override def extractLong[ALG2[_], A](
    op: ALG2[A]
  )(in: JValue, path: List[String]): Either[ExtractionErrors[String], Long] = {
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

  override def extractShort[ALG2[_], A](
    op: ALG2[A]
  )(in: JValue, path: List[String]): Either[ExtractionErrors[String], Short] = {
    in match {
      case JInt(i)     => tryNumberConversion[BigInt, Short](path, i, _.toShort)
      case JLong(l)    => tryNumberConversion[Long, Short](path, l, _.toShort)
      case JDecimal(d) => tryNumberConversion[BigDecimal, Short](path, d, _.toShort)
      case JDouble(d)  => tryNumberConversion[Double, Short](path, d, _.toShort)
      case _ =>
        Left(List(WrongTypeError(path, "Short", in.getClass.getSimpleName, None)))
    }
  }

  override def extractBool[ALG2[_], A](
    op: ALG2[A]
  )(in: JValue, path: List[String]): Either[ExtractionErrors[String], Boolean] = {
    in match {
      case JBool(bool) => Right(bool)
      case _           => Left(determineError(in, op, "Boolean", path))
    }
  }

  override def extractArray[ALG2[_], A](
    op: ListData[String, ALG2, A]
  )(in: JValue, path: Path[String]): Either[ExtractionErrors[String], Seq[JValue]] = {
    in match {
      case JArray(elements) => Right(elements)
      case _                => Left(determineError(in, Left(op), op.typeNameOfT, path))
    }
  }

  override def extractBigDecimal[ALG2[_], A](
    op: ALG2[A]
  )(in: JValue, path: List[String]): Either[ExtractionErrors[String], BigDecimal] = {
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
    op: ALG2[A],
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
