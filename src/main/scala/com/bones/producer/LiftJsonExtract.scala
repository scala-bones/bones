package com.bones.producer

import cats.data.Validated
import cats.data.Validated.{Invalid, Valid}
import com.bones.data.Key
import com.bones.interpreter.ExtractionInterpreter.WrongTypeError
import com.bones.json.JsonExtract
import net.liftweb.json.JsonAST._

case class LiftJsonExtract(jValue: JValue) extends JsonExtract {

  private def lookForString(jValue: JValue): Validated[WrongTypeError[String], Option[String]] = {
    jValue match {
      case JArray(_) => Invalid(WrongTypeError( classOf[String], classOf[JArray]))
      case JBool(_) => Invalid(WrongTypeError( classOf[String], classOf[JBool]))
      case JDouble(_) => Invalid(WrongTypeError( classOf[String], classOf[JDouble]))
//      case JField(_,JString(s)) => Valid(Some(s))
      case JInt(_) => Invalid(WrongTypeError( classOf[String], classOf[JInt]))
      case JNothing => Valid(None)
      case JObject(_) => Invalid(WrongTypeError( classOf[String], classOf[JObject]))
      case JString(str) => Valid(Some(str))
      case JNull =>  Valid(None)
    }
  }

  private def lookForDouble(jValue: JValue): Validated[WrongTypeError[Double], Option[Double]] = {
    jValue match {
      case JArray(_) => Invalid(WrongTypeError( classOf[Double], classOf[JArray]))
      case JBool(_) => Invalid(WrongTypeError( classOf[Double], classOf[JBool]))
      case JDouble(d) => Valid(Some(d))
//      case JField(_,_) => Invalid(WrongTypeError( classOf[Double], classOf[JField]))
      case JInt(_) => Invalid(WrongTypeError( classOf[Double], classOf[JInt]))
      case JNothing => Valid(None)
      case JObject(_) => Invalid(WrongTypeError( classOf[Double], classOf[JObject]))
      case JString(str) => Invalid(WrongTypeError( classOf[Double], classOf[JString]))
      case JNull =>  Valid(None)
    }
  }

  private def lookForInt(jValue: JValue): Validated[WrongTypeError[Int], Option[Int]] = {
    jValue match {
      case JArray(_) => Invalid(WrongTypeError( classOf[Int], classOf[Array[_]]))
      case JBool(_) => Invalid(WrongTypeError( classOf[Int], classOf[Boolean]))
      case JDouble(_) => Invalid(WrongTypeError( classOf[Int], classOf[Double]))
//      case JField(_,_) => Invalid(WrongTypeError( classOf[Int], classOf[Object]))
      case JInt(i) => Valid(Some(i.intValue()))
      case JNothing => Valid(None)
      case JObject(_) => Invalid(WrongTypeError( classOf[Int], classOf[Object]))
      case JString(_) => Invalid(WrongTypeError( classOf[Int], classOf[String]))
      case JNull =>  Valid(None)
    }
  }

  private def lookForObject(jValue: JValue): Validated[WrongTypeError[JsonExtract], Option[LiftJsonExtract]] = {
    jValue match {
      case JArray(_) => Invalid(WrongTypeError( classOf[JsonExtract], classOf[JArray]))
      case JBool(_) => Invalid(WrongTypeError( classOf[JsonExtract], classOf[JBool]))
      case JDouble(_) => Invalid(WrongTypeError( classOf[JsonExtract], classOf[JDouble]))
//      case JField(_,_) => Invalid(WrongTypeError( classOf[JsonExtract], classOf[JField]))
      case JInt(_) => Invalid(WrongTypeError( classOf[JsonExtract], classOf[JInt]))
      case JNothing => Valid(None)
      case JObject(obj) => Valid(Some(LiftJsonExtract(JObject(obj))))
      case JString(_) => Invalid(WrongTypeError( classOf[JsonExtract], classOf[JString]))
      case JNull =>  Valid(None)
    }
  }

  private def lookForBool(jValue: JValue): Validated[WrongTypeError[Boolean], Option[Boolean]] = {
    jValue match {
      case JArray(_) => Invalid(WrongTypeError( classOf[Boolean], classOf[Array[_]]))
      case JBool(b) => Valid(Some(b))
      case JDouble(_) => Invalid(WrongTypeError( classOf[Boolean], classOf[Double]))
//      case JField(_,_) => Invalid(WrongTypeError( classOf[Boolean], classOf[Object]))
      case JInt(_) => Invalid(WrongTypeError( classOf[Boolean], classOf[BigInt]))
      case JNothing => Valid(None)
      case JObject(_) => Invalid(WrongTypeError( classOf[Boolean], classOf[BigInt]))
      case JString(_) => Invalid(WrongTypeError( classOf[Boolean], classOf[String]))
      case JNull =>  Valid(None)
    }
  }

  private def lookForArray(jValue: JValue): Validated[WrongTypeError[List[_]], Option[List[LiftJsonExtract]]] = {
    jValue match {
      case a: JArray => Valid(Some(a.arr.map(child => LiftJsonExtract(child))))
      case JNothing => Valid(None)
      case JNull => Valid(None)
      case JDouble(_) => Invalid(WrongTypeError( classOf[List[_]], classOf[Double]))
//      case JField(_,_) => Invalid(WrongTypeError( classOf[List[_]], classOf[Object]))
      case JInt(_) => Invalid(WrongTypeError( classOf[List[_]], classOf[BigInt]))
      case JObject(_) => Invalid(WrongTypeError( classOf[List[_]], classOf[BigInt]))
      case JString(_) => Invalid(WrongTypeError( classOf[List[_]], classOf[String]))
      case JBool(_) => Invalid(WrongTypeError( classOf[List[_]], classOf[Boolean]))
    }
  }

  override def extractDouble: Validated[WrongTypeError[Double], Option[Double]] =
    lookForDouble(jValue)


  override def extractString: Validated[WrongTypeError[String], Option[String]] =
    lookForString(jValue)

  override def extractInt: Validated[WrongTypeError[Int], Option[Int]] = lookForInt(jValue)

  override def extractObject: Validated[WrongTypeError[JsonExtract], Option[LiftJsonExtract]] =
    lookForObject(jValue)

  override def extractBool: Validated[WrongTypeError[Boolean], Option[Boolean]] = lookForBool(jValue)

  override def extractList: Validated[WrongTypeError[List[_]], Option[List[JsonExtract]]] =
    lookForArray(jValue)

  override def child(key: Key): LiftJsonExtract = LiftJsonExtract( jValue \ key.name )
}
