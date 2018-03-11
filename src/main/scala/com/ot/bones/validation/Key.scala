package com.ot.bones.validation

import com.ot.bones.BooleanDataDefinition.BooleanData
import com.ot.bones.IntDataDefinition.IntData
import com.ot.bones.StringDataDefinition.StringData
import com.ot.bones.ToHList.HList2
import com.ot.bones.validation.ValidationDefinition.{ToOptionalValidation, ValidationOp}
import shapeless.{::, HNil}

trait KeySyntax {
  /** Turn a string key into an key type */
  def key(key: String) = Key(key)
  implicit class StringToKey(str: String) {
    def key(): Key = Key(str)
  }
}

trait FieldDefinition[A, +D<:DataDefinitionOp[A]] {
  val key: Key
  val op: D
  val validations: List[ValidationOp[A]]
}

case class OptionalFieldDefinition[A, +D<:DataDefinitionOp[A]](key: Key, op: D, validations: List[ValidationOp[A]])
  extends FieldDefinition[A,D]

case class RequiredFieldDefinition[A, +D<:DataDefinitionOp[A] with ToOptionalData[A]](key: Key, op: D, validations: List[ValidationOp[A] with ToOptionalValidation[A]])
  extends FieldDefinition[A,D] {
  def optional() = {
    val optionalValidations = validations.map(_.toOption)
    OptionalFieldDefinition(key, op.toOption, optionalValidations)
  }
}

/** Starting point for obtaining a value is to define a key */
case class Key(name: String) { thisKey =>
  val key: Key = thisKey
  def string() : FieldDefinition[String, StringData] = RequiredFieldDefinition(this, StringData(), List.empty)
  def string(f: ValidationOp[String] with ToOptionalValidation[String] *): FieldDefinition[String, StringData] = RequiredFieldDefinition(this, StringData(), f.toList)
  /** Use this if you expect the int to come in as a JSON number, otherwise use string().int() */
  def int(): FieldDefinition[Int, IntData] = RequiredFieldDefinition[Int, IntData](this, IntData(), List.empty)
  def int(f: ValidationOp[Int] with ToOptionalValidation[Int] *): FieldDefinition[Int, IntData] = RequiredFieldDefinition[Int, IntData](this, IntData(), f.toList)
  /** Use this if you expect the bigDecimal to come in as a JSON number, otherwise use string().bigDecimal */
  //    def either[A,B](v1: ValidationOp[A], v2: ValidationOp[B]): Extract[Either[A,B]] = new Extract[Either[A,B]]{
  //      override def validation = CanBeEither[A,B](v1, v2)
  //      override val key = thisKey
  //    }
  //def vector(): Extract[Vector[Int]] = ???
//  def list[T](dataDefinitionOp: DataDefinitionOp[T]): RequiredList[T] = RequiredList(this, dataDefinitionOp)
  def boolean(): FieldDefinition[Boolean, BooleanData] = RequiredFieldDefinition[Boolean, BooleanData](this, BooleanData(), List.empty)
  def boolean(f: ValidationOp[Boolean] with ToOptionalValidation[Boolean] *): FieldDefinition[Boolean, BooleanData] = RequiredFieldDefinition[Boolean, BooleanData](this, BooleanData(), f.toList)


  def obj2[A, B](op1: FieldDefinition[A, DataDefinitionOp[A]], op2: FieldDefinition[B, DataDefinitionOp[B]]) =
    RequiredFieldDefinition[A :: B :: HNil, HList2[A,B]](this, HList2(op1, op2), List.empty)
}



