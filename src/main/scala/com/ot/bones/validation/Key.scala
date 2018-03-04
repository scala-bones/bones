package com.ot.bones.validation

import com.ot.bones.BooleanDataDefinition.RequiredBoolean
import com.ot.bones.IntDataDefinition.RequiredInt
import com.ot.bones.StringDataDefinition.RequiredString
import com.ot.bones.ToHList.{HList2, ToHListDataDefinitionOp}
import com.ot.bones.validation.ValidationDefinition.{Validation, ValidationOp}
import shapeless.{::, HList, HNil}

trait KeySyntax {
  /** Turn a string key into an key type */
  def key(key: String) = Key(key)
  implicit class StringToKey(str: String) {
    def key(): Key = Key(str)
  }
}

case class FieldDefinition[A, +D<:DataDefinitionOp[A]](key: Key, op: D, validations: List[ValidationOp[A]], required: Boolean = true) {
  def optional() = FieldDefinition(key, op, validations, false)
}

/** Starting point for obtaining a value is to define a key */
case class Key(name: String) { thisKey =>
  val key: Key = thisKey
  def string() : FieldDefinition[String, RequiredString] = FieldDefinition(this, RequiredString(), List.empty)
  def string(f: ValidationOp[String]*): FieldDefinition[String, RequiredString] = FieldDefinition(this, RequiredString(), f.toList)
  /** Use this if you expect the int to come in as a JSON number, otherwise use string().int() */
  def int(): FieldDefinition[Int, RequiredInt] = FieldDefinition[Int, RequiredInt](this, RequiredInt(), List.empty)
  def int(f: ValidationOp[Int]*): FieldDefinition[Int, RequiredInt] = FieldDefinition[Int, RequiredInt](this, RequiredInt(), f.toList)
  /** Use this if you expect the bigDecimal to come in as a JSON number, otherwise use string().bigDecimal */
  //    def either[A,B](v1: ValidationOp[A], v2: ValidationOp[B]): Extract[Either[A,B]] = new Extract[Either[A,B]]{
  //      override def validation = CanBeEither[A,B](v1, v2)
  //      override val key = thisKey
  //    }
  //def vector(): Extract[Vector[Int]] = ???
//  def list[T](dataDefinitionOp: DataDefinitionOp[T]): RequiredList[T] = RequiredList(this, dataDefinitionOp)
  def boolean(): FieldDefinition[Boolean, RequiredBoolean] = FieldDefinition[Boolean, RequiredBoolean](this, RequiredBoolean(), List.empty)
  def boolean(f: ValidationOp[Boolean]*): FieldDefinition[Boolean, RequiredBoolean] = FieldDefinition[Boolean, RequiredBoolean](this, RequiredBoolean(), f.toList)


  def obj2[A, B](op1: FieldDefinition[A, DataDefinitionOp[A]], op2: FieldDefinition[B, DataDefinitionOp[B]]) =
    FieldDefinition[A :: B :: HNil, ToHListDataDefinitionOp[A :: B :: HNil]](this, HList2(op1, op2), List.empty)
}



