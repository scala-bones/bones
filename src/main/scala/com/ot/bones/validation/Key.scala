package com.ot.bones.validation

import com.ot.bones.BooleanDataDefinition.RequiredBoolean
import com.ot.bones.IntDataDefinition.RequiredInt
import com.ot.bones.StringDataDefinition.RequiredString
import com.ot.bones.ToHList.HList2
import com.ot.bones.validation.ValidationDefinition.{Validation, ValidationOp}

trait KeySyntax {
  /** Turn a string key into an key type */
  def key(key: String) = Key(key)
  implicit class StringToKey(str: String) {
    def key(): Key = Key(str)
  }
}

case class KeyAndData[A, D<:DataDefinitionOp[A]](key: Key, op: D) {
  def convert[B](converter: Converter[A,B])
}

/** Starting point for obtaining a value is to define a key */
case class Key(name: String) { thisKey =>
  val key: Key = thisKey
  def string() : (Key, RequiredString) = (this, RequiredString(List.empty))
//  def string(f: RequiredString => RequiredString) : (Key, RequiredString) = (this, f(RequiredString(List.empty)))
  def string(f: ValidationOp[String]*): (Key, RequiredString) = (this, RequiredString(f.toList))
  /** Use this if you expect the int to come in as a JSON number, otherwise use string().int() */
  def int(): (Key, RequiredInt) = (this, RequiredInt(List.empty))
  def int(f: ValidationOp[Int]*): (Key, RequiredInt) = (this, RequiredInt(f.toList))
  /** Use this if you expect the bigDecimal to come in as a JSON number, otherwise use string().bigDecimal */
  //    def either[A,B](v1: ValidationOp[A], v2: ValidationOp[B]): Extract[Either[A,B]] = new Extract[Either[A,B]]{
  //      override def validation = CanBeEither[A,B](v1, v2)
  //      override val key = thisKey
  //    }
  //def vector(): Extract[Vector[Int]] = ???
//  def list[T](dataDefinitionOp: DataDefinitionOp[T]): RequiredList[T] = RequiredList(this, dataDefinitionOp)
  def boolean(): (Key, RequiredBoolean) = (this, RequiredBoolean())

  def obj2[A, B](op1: (Key, DataDefinitionOp[A]), op2: (Key, DataDefinitionOp[B])) = (this, HList2(op1, op2))
}



