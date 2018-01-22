package com.ot.bones.producer

import cats.data.Validated
import cats.data.Validated.{Invalid, Valid}
import com.ot.bones.interpreter.ExtractionInterpreter.{JsonProducer, WrongTypeError}
import com.ot.bones.validation.{Key, RootKey, StringKey}
import net.liftweb.json.JsonAST._

case class LiftJsonProducer(jValue: JValue) extends JsonProducer {

  private def lookForString(key: Key, jValue: JValue): Validated[WrongTypeError[String], Option[String]] = {
    jValue match {
      case JArray(_) => Invalid(WrongTypeError(key, classOf[String], classOf[Array[_]]))
      case JBool(_) => Invalid(WrongTypeError(key, classOf[String], classOf[Boolean]))
      case JDouble(_) => Invalid(WrongTypeError(key, classOf[String], classOf[Double]))
      case JField(_,_) => Invalid(WrongTypeError(key, classOf[String], classOf[Object]))
      case JInt(_) => Invalid(WrongTypeError(key, classOf[String], classOf[BigInt]))
      case JNothing => Valid(None)
      case JObject(_) => Invalid(WrongTypeError(key, classOf[String], classOf[Object]))
      case JString(str) => Valid(Some(str))
      case JNull =>  Valid(None)
    }
  }

  private def lookForBigDecimal(key: Key, jValue: JValue): Validated[WrongTypeError[BigDecimal], Option[BigDecimal]] = {
    jValue match {
      case JArray(_) => Invalid(WrongTypeError(key, classOf[BigDecimal], classOf[Array[_]]))
      case JBool(_) => Invalid(WrongTypeError(key, classOf[BigDecimal], classOf[Boolean]))
      case JDouble(d) => Valid(Some(BigDecimal(d)))
      case JField(_,_) => Invalid(WrongTypeError(key, classOf[BigDecimal], classOf[Object]))
      case JInt(_) => Invalid(WrongTypeError(key, classOf[BigDecimal], classOf[BigInt]))
      case JNothing => Valid(None)
      case JObject(_) => Invalid(WrongTypeError(key, classOf[BigDecimal], classOf[Object]))
      case JString(str) => try {
        Valid(Some(BigDecimal(str)))
      } catch {
        case _: NumberFormatException => Invalid(WrongTypeError(key, classOf[BigDecimal], classOf[Object]))
      }
      case JNull =>  Valid(None)
    }
  }

  private def lookForInt(key: Key, jValue: JValue): Validated[WrongTypeError[Int], Option[Int]] = {
    jValue match {
      case JArray(_) => Invalid(WrongTypeError(key, classOf[Int], classOf[Array[_]]))
      case JBool(_) => Invalid(WrongTypeError(key, classOf[Int], classOf[Boolean]))
      case JDouble(_) => Invalid(WrongTypeError(key, classOf[Int], classOf[Double]))
      case JField(_,_) => Invalid(WrongTypeError(key, classOf[Int], classOf[Object]))
      case JInt(i) => Valid(Some(i.intValue()))
      case JNothing => Valid(None)
      case JObject(_) => Invalid(WrongTypeError(key, classOf[Int], classOf[Object]))
      case JString(_) => Invalid(WrongTypeError(key, classOf[Int], classOf[String]))
      case JNull =>  Valid(None)
    }
  }

  private def lookForObject(key: Key, jValue: JValue): Validated[WrongTypeError[JsonProducer], Option[LiftJsonProducer]] = {
    jValue match {
      case JArray(_) => Invalid(WrongTypeError(key, classOf[JsonProducer], classOf[Array[_]]))
      case JBool(_) => Invalid(WrongTypeError(key, classOf[JsonProducer], classOf[Boolean]))
      case JDouble(_) => Invalid(WrongTypeError(key, classOf[JsonProducer], classOf[Double]))
      case JField(_,_) => Invalid(WrongTypeError(key, classOf[JsonProducer], classOf[Object]))
      case JInt(_) => Invalid(WrongTypeError(key, classOf[JsonProducer], classOf[BigInt]))
      case JNothing => Valid(None)
      case JObject(obj) => Valid(Some(LiftJsonProducer(JObject(obj))))
      case JString(_) => Invalid(WrongTypeError(key, classOf[JsonProducer], classOf[String]))
      case JNull =>  Valid(None)
    }
  }

  private def lookForBool(key: Key, jValue: JValue): Validated[WrongTypeError[Boolean], Option[Boolean]] = {
    jValue match {
      case JArray(_) => Invalid(WrongTypeError(key, classOf[Boolean], classOf[Array[_]]))
      case JBool(b) => Valid(Some(b))
      case JDouble(_) => Invalid(WrongTypeError(key, classOf[Boolean], classOf[Double]))
      case JField(_,_) => Invalid(WrongTypeError(key, classOf[Boolean], classOf[Object]))
      case JInt(_) => Invalid(WrongTypeError(key, classOf[Boolean], classOf[BigInt]))
      case JNothing => Valid(None)
      case JObject(_) => Invalid(WrongTypeError(key, classOf[Boolean], classOf[BigInt]))
      case JString(_) => Invalid(WrongTypeError(key, classOf[Boolean], classOf[String]))
      case JNull =>  Valid(None)
    }
  }

  private def lookForArray(key: Key, jValue: JValue): Validated[WrongTypeError[List[_]], Option[List[LiftJsonProducer]]] = {
    jValue match {
      case a: JArray => Valid(Some(a.arr.map(child => LiftJsonProducer(child))))
      case JNothing => Valid(None)
      case JNull => Valid(None)
      case JDouble(_) => Invalid(WrongTypeError(key, classOf[List[_]], classOf[Double]))
      case JField(_,_) => Invalid(WrongTypeError(key, classOf[List[_]], classOf[Object]))
      case JInt(_) => Invalid(WrongTypeError(key, classOf[List[_]], classOf[BigInt]))
      case JObject(_) => Invalid(WrongTypeError(key, classOf[List[_]], classOf[BigInt]))
      case JString(_) => Invalid(WrongTypeError(key, classOf[List[_]], classOf[String]))
      case JBool(_) => Invalid(WrongTypeError(key, classOf[List[_]], classOf[Boolean]))
    }
  }

  override def produceBigDecimal(key: Key): Validated[WrongTypeError[BigDecimal], Option[BigDecimal]] = key match {
    case RootKey => lookForBigDecimal(key, jValue)
    case StringKey(field) => lookForBigDecimal(key, jValue \ field)
  }


  override def produceString(key: Key): Validated[WrongTypeError[String], Option[String]] = key match {
    case RootKey => lookForString(key, jValue)
    case StringKey(field) => lookForString(key, jValue \ field)
  }

  override def produceInt(key: Key): Validated[WrongTypeError[Int], Option[Int]] = key match {
    case RootKey => lookForInt(key, jValue)
    case StringKey(field) => lookForInt(key, jValue \ field)
  }

  override def produceObject(key: Key): Validated[WrongTypeError[JsonProducer], Option[LiftJsonProducer]] = key match {
    case RootKey => lookForObject(key, jValue)
    case StringKey(field) => lookForObject(key, jValue \ field)
  }


  override def produceBool(key: Key): Validated[WrongTypeError[Boolean], Option[Boolean]] = key match {
    case RootKey => lookForBool(key, jValue)
    case StringKey(field) => lookForBool(key, jValue \ field)
  }

  override def produceList(key: Key): Validated[WrongTypeError[List[_]], Option[List[JsonProducer]]] =  key match {
    case RootKey => lookForArray(key, jValue)
    case StringKey(field) => lookForArray(key, jValue \ field)
  }
}
