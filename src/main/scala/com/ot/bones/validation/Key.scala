package com.ot.bones.validation

import com.ot.bones.interpreter.ExtractionInterpreter.JsonProducer
import com.ot.bones.validation.IntValidation.RequiredInt
import com.ot.bones.validation.StringValidation.RequiredString

trait KeySyntax {
  /** Turn a string key into an key type */
  def key(key: String) = StringKey(key)
  implicit class StringToKey(str: String) {
    def key(): Key = StringKey(str)
  }
}

/** Starting point for obtaining a value is to define a key */
sealed abstract class Key extends DataDefinitionOp[Option[JsonProducer]] with ObjAlias { thisKey =>
  val key = thisKey
  def string() : RequiredString = RequiredString(thisKey, Nil)
  def int(): RequiredInt = RequiredInt(thisKey, Nil)
  //    def BigDecimal(): Extract[Int] = ???
  //    def either[A,B](v1: ValidationOp[A], v2: ValidationOp[B]): Extract[Either[A,B]] = new Extract[Either[A,B]]{
  //      override def validation = CanBeEither[A,B](v1, v2)
  //      override val key = thisKey
  //    }
  //    def array(): Extract[Vector[Int]] = ???
  //    def boolean(): Extract[Boolean] = ???
  //    def binary(): Extract[Boolean] = ???  //maybe this is a string().binary().
  //    def date(): Extract[Date] = ???
}

object RootKey extends Key
case class StringKey(name: String) extends Key

