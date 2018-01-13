package com.ot.bones.producer

import com.ot.bones
import com.ot.bones.compiler.ExtractionCompiler.{JsonProducer, WrongTypeError}
import com.ot.bones.{Key, RootKey, StringKey}
import net.liftweb.json.JsonAST._

case class LiftJson(jValue: JValue) extends JsonProducer {

  private def lookForString(key: Key, jValue: JValue): Either[WrongTypeError[String], Option[String]] = {
    jValue match {
      case JArray(_) => Left(WrongTypeError(key, classOf[String], classOf[Array[_]]))
      case JBool(_) => Left(WrongTypeError(key, classOf[String], classOf[Boolean]))
      case JDouble(_) => Left(WrongTypeError(key, classOf[String], classOf[Double]))
      case JField(_,_) => Left(WrongTypeError(key, classOf[String], classOf[Object]))
      case JInt(_) => Left(WrongTypeError(key, classOf[String], classOf[BigInt]))
      case JNothing => Right(None)
      case JObject(_) => Left(WrongTypeError(key, classOf[String], classOf[Object]))
      case JString(str) => Right(Some(str))
      case JNull =>  Right(None)
    }
  }

  private def lookForBigDecimal(key: Key, jValue: JValue): Either[WrongTypeError[BigDecimal], Option[BigDecimal]] = {
    jValue match {
      case JArray(_) => Left(WrongTypeError(key, classOf[BigDecimal], classOf[Array[_]]))
      case JBool(_) => Left(WrongTypeError(key, classOf[BigDecimal], classOf[Boolean]))
      case JDouble(d) => Right(Some(BigDecimal(d)))
      case JField(_,_) => Left(WrongTypeError(key, classOf[BigDecimal], classOf[Object]))
      case JInt(_) => Left(WrongTypeError(key, classOf[BigDecimal], classOf[BigInt]))
      case JNothing => Right(None)
      case JObject(_) => Left(WrongTypeError(key, classOf[BigDecimal], classOf[Object]))
      case JString(str) => try {
        Right(Some(BigDecimal(str)))
      } catch {
        case _: NumberFormatException => Left(WrongTypeError(key, classOf[BigDecimal], classOf[Object]))
      }
      case JNull =>  Right(None)
    }
  }

  private def lookForInt(key: Key, jValue: JValue): Either[WrongTypeError[Int], Option[Int]] = {
    jValue match {
      case JArray(_) => Left(WrongTypeError(key, classOf[Int], classOf[Array[_]]))
      case JBool(_) => Left(WrongTypeError(key, classOf[Int], classOf[Boolean]))
      case JDouble(_) => Left(WrongTypeError(key, classOf[Int], classOf[Double]))
      case JField(_,_) => Left(WrongTypeError(key, classOf[Int], classOf[Object]))
      case JInt(i) => Right(Some(i.intValue()))
      case JNothing => Right(None)
      case JObject(_) => Left(WrongTypeError(key, classOf[Int], classOf[Object]))
      case JString(_) => Left(WrongTypeError(key, classOf[Int], classOf[String]))
      case JNull =>  Right(None)
    }
  }

  private def lookForObject(key: Key, jValue: JValue): Either[WrongTypeError[JsonProducer], Option[LiftJson]] = {
    jValue match {
      case JArray(_) => Left(WrongTypeError(key, classOf[JsonProducer], classOf[Array[_]]))
      case JBool(_) => Left(WrongTypeError(key, classOf[JsonProducer], classOf[Boolean]))
      case JDouble(_) => Left(WrongTypeError(key, classOf[JsonProducer], classOf[Double]))
      case JField(_,_) => Left(WrongTypeError(key, classOf[JsonProducer], classOf[Object]))
      case JInt(_) => Left(WrongTypeError(key, classOf[JsonProducer], classOf[BigInt]))
      case JNothing => Right(None)
      case JObject(obj) => Right(Some(LiftJson(JObject(obj))))
      case JString(_) => Left(WrongTypeError(key, classOf[JsonProducer], classOf[String]))
      case JNull =>  Right(None)
    }
  }

  private def lookForBool(key: Key, jValue: JValue): Either[WrongTypeError[Boolean], Option[Boolean]] = {
    jValue match {
      case JArray(_) => Left(WrongTypeError(key, classOf[Boolean], classOf[Array[_]]))
      case JBool(b) => Right(Some(b))
      case JDouble(_) => Left(WrongTypeError(key, classOf[Boolean], classOf[Double]))
      case JField(_,_) => Left(WrongTypeError(key, classOf[Boolean], classOf[Object]))
      case JInt(_) => Left(WrongTypeError(key, classOf[Boolean], classOf[BigInt]))
      case JNothing => Right(None)
      case JObject(_) => Left(WrongTypeError(key, classOf[Boolean], classOf[BigInt]))
      case JString(_) => Left(WrongTypeError(key, classOf[Boolean], classOf[String]))
      case JNull =>  Right(None)
    }
  }

  override def produceBigDecimal(key: bones.Key): Either[WrongTypeError[BigDecimal], Option[BigDecimal]] = key match {
    case RootKey => lookForBigDecimal(key, jValue)
    case StringKey(field) => lookForBigDecimal(key, jValue \ field)
  }


  override def produceString(key: bones.Key): Either[WrongTypeError[String], Option[String]] = key match {
    case RootKey => lookForString(key, jValue)
    case StringKey(field) => lookForString(key, jValue \ field)
  }

  override def produceInt(key: bones.Key): Either[WrongTypeError[Int], Option[Int]] = key match {
    case RootKey => lookForInt(key, jValue)
    case StringKey(field) => lookForInt(key, jValue \ field)
  }

  override def produceObject(key: bones.Key): Either[WrongTypeError[JsonProducer], Option[LiftJson]] = key match {
    case RootKey => lookForObject(key, jValue)
    case StringKey(field) => lookForObject(key, jValue \ field)
  }


  override def produceBool(key: bones.Key): Either[WrongTypeError[Boolean], Option[Boolean]] = key match {
    case RootKey => lookForBool(key, jValue)
    case StringKey(field) => lookForBool(key, jValue \ field)
  }
}
