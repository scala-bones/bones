package com.bones.json

import cats.data.Validated
import com.bones.data.Key
import com.bones.interpreter.ExtractionInterpreter.WrongTypeError

trait JsonCreate {

  def createString(value: String): JsonCreate
  def createInt(value: Int): JsonCreate
  def extractBool(bool: Boolean): JsonCreate
  def extractDouble(double: Double): JsonCreate
  def extractObject (key: String) : JsonCreate
  def extractList: Validated[WrongTypeError[List[_]], Option[List[JsonExtract]]]
  def child(key: Key): JsonExtract

}
