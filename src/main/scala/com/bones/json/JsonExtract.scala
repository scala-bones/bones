package com.bones.json

import cats.data.Validated
import com.bones.data.Key
import com.bones.interpreter.ExtractionInterpreter.WrongTypeError

/** Simple little interface around a Json library.
  * I believe it may be best to actually write an extractor
  * to and from your preferred library instead of extending JsonExtract, however this is here how I first implemented this,
  * so I'll keep this around for now.
  **/
trait JsonExtract {
  def extractString: Validated[WrongTypeError[String], Option[String]]
  def extractInt: Validated[WrongTypeError[Int], Option[Int]]
  def extractBool: Validated[WrongTypeError[Boolean], Option[Boolean]]
  def extractDouble: Validated[WrongTypeError[Double], Option[Double]]
  def extractObject: Validated[WrongTypeError[JsonExtract], Option[JsonExtract]]
  def extractList: Validated[WrongTypeError[List[_]], Option[List[JsonExtract]]]
  def child(key: Key): JsonExtract
}

