package com.ot.bones.data

import java.text.{DateFormat, SimpleDateFormat}
import java.util.{Date, UUID}

import cats.data.Validated
import com.ot.bones.data.Algebra._
import com.ot.bones.data.ToHList._
import com.ot.bones.interpreter.ExtractionInterpreter.ConversionError
import com.ot.bones.validation.ValidationDefinition.{ToOptionalValidation, ValidationOp}
import shapeless.{::, HNil}

trait KeySyntax {
  /** Turn a string key into an key type */
  def key(key: String) = Key(key)
  implicit class StringToKey(str: String) {
    def key(): Key = Key(str)
  }
}

/**
  * Base type for a field definition.
  * @tparam A The type this field definition is describing.
  */
trait FieldDefinition[A] {
  /** String key, aka field name */
  val key: Key
  /** One of the Data Definitions describing the data of this field. */
  val op: DataDefinitionOp[A]
  /** List of validations this field should adhere to*/
  val validations: List[ValidationOp[A]]
}

/** Indicates that the field is Optional */
case class OptionalFieldDefinition[A](key: Key, op: DataDefinitionOp[A], validations: List[ValidationOp[A]])
  extends FieldDefinition[A]

/** Indicates that the field is required */
case class RequiredFieldDefinition[A](key: Key,
                                      op: DataDefinitionOp[A]  with ToOptionalData[A],
                                      validations: List[ValidationOp[A] with ToOptionalValidation[A]])
  extends FieldDefinition[A] {
  def optional(): OptionalFieldDefinition[Option[A]] = {
    val optionalValidations = validations.map(_.toOption)
    OptionalFieldDefinition(key, op.toOption, optionalValidations)
  }

  def convert[B](fab: A => Validated[ConversionError[A,B], B], fba: B => A, description: String, validations: List[ValidationOp[B]]) = {
    val cd = ConversionData(op, fab, fba, description)
    ConversionFieldDefinition(this, cd, validations)
  }

}

case class ConversionFieldDefinition[A,B](convertFrom: RequiredFieldDefinition[A], op: ConversionData[A,B], validations: List[ValidationOp[B]])
  extends FieldDefinition[B] {
  /** String key, aka field name */
  override val key: Key = convertFrom.key
}

/** Starting point for obtaining a value is to define a key */
case class Key(name: String) { thisKey =>
  val key: Key = thisKey
  def string() : RequiredFieldDefinition[String] = RequiredFieldDefinition(this, StringData(), List.empty)
  def string(f: ValidationOp[String] with ToOptionalValidation[String] *): RequiredFieldDefinition[String] = RequiredFieldDefinition(this, StringData(), f.toList)
  /** Use this if you expect the int to come in as a JSON number, otherwise use string().int() */
  def int(): RequiredFieldDefinition[Int] = RequiredFieldDefinition[Int](this, IntData(), List.empty)
  def int(f: ValidationOp[Int] with ToOptionalValidation[Int] *): RequiredFieldDefinition[Int] = RequiredFieldDefinition[Int](this, IntData(), f.toList)
  /** Use this if you expect the bigDecimal to come in as a JSON number, otherwise use string().bigDecimal */
  //    def either[A,B](v1: ValidationOp[A], v2: ValidationOp[B]): Extract[Either[A,B]] = new Extract[Either[A,B]]{
  //      override def validation = CanBeEither[A,B](v1, v2)
  //      override val key = thisKey
  //    }
  //def vector(): Extract[Vector[Int]] = ???
  def list[T](dataDefinitionOp: DataDefinitionOp[T]): RequiredFieldDefinition[List[T]] = RequiredFieldDefinition(this, ListData(dataDefinitionOp), List.empty)
  def boolean(): RequiredFieldDefinition[Boolean] = RequiredFieldDefinition[Boolean](this, BooleanData(), List.empty)
  def boolean(f: ValidationOp[Boolean] with ToOptionalValidation[Boolean] *): RequiredFieldDefinition[Boolean] =
    RequiredFieldDefinition[Boolean](this, BooleanData(), f.toList)
  def uuid(): RequiredFieldDefinition[UUID] = RequiredFieldDefinition(this, UuidData(), List.empty)
  /** Date, BYOFormat */
  def date(dateFormat: DateFormat, formatDescription: Option[String] = None): RequiredFieldDefinition[Date] =
    RequiredFieldDefinition(key, DateData(dateFormat, formatDescription), List.empty)

  /** Expecting a string that is in the format of an iso date time */
  def isoDateTime(): RequiredFieldDefinition[Date] =
    RequiredFieldDefinition(key,
      DateData(
        new SimpleDateFormat("yyyy-MM-dd'T'HH:mm'Z'"),
        Some("ISO date-time format with the offset and zone if available, such as '2011-12-03T10:15:30', '2011-12-03T10:15:30+01:00' or '2011-12-03T10:15:30+01:00[Europe/Paris]'")
      ),
      List.empty
    )


  /** Expecting a string that is in the format of an iso date */
  def isoDate(): RequiredFieldDefinition[Date] = RequiredFieldDefinition(key, isoDateData, List.empty)
  private val isoDateData = DateData(
    new SimpleDateFormat("yyyy-MM-dd"),
    Some("ISO date format with the offset if available, such as '2011-12-03' or '2011-12-03+01:00'")
  )



  def obj2[A, B](op1: FieldDefinition[A], op2: FieldDefinition[B]) =
    RequiredFieldDefinition[A :: B :: HNil](this, HList2(op1, op2), List.empty)
}

object Sugar {
  /** Aliases for simplifying the creation of ObjectFieldGroup. */
  /** Convenient ways to declare Object specification */
  trait ToHList {

    def obj2[A, AA, B, BB](op1: FieldDefinition[A],
                           op2: FieldDefinition[B]) = HList2(op1, op2)

    def obj3[A, B, C](op1: FieldDefinition[A], op2: FieldDefinition[B], op3: FieldDefinition[C]) =
      HList3(op1, op2, op3)

    def obj4[A, B, C, D](op1: FieldDefinition[A], op2: FieldDefinition[B], op3: FieldDefinition[C], op4: FieldDefinition[D]) =
      HList4(op1, op2, op3, op4)

    def obj5[A, B, C, D, E](op1: FieldDefinition[A], op2: FieldDefinition[B], op3: FieldDefinition[C], op4: FieldDefinition[D], op5: FieldDefinition[E]) =
      HList5(op1, op2, op3, op4, op5)

    def obj6[A, B, C, D, E, F](op1: FieldDefinition[A], op2: FieldDefinition[B], op3: FieldDefinition[C], op4: FieldDefinition[D], op5: FieldDefinition[E],
                               op6: FieldDefinition[F]) =
      HList6(op1, op2, op3, op4, op5, op6)

    def obj7[A, B, C, D, E, F, G](op1: FieldDefinition[A], op2: FieldDefinition[B], op3: FieldDefinition[C], op4: FieldDefinition[D], op5: FieldDefinition[E],
                                  op6: FieldDefinition[F], op7: FieldDefinition[G]) =
      HList7(op1, op2, op3, op4, op5, op6, op7)

    def obj8[A, B, C, D, E, F, G, H](op1: FieldDefinition[A], op2: FieldDefinition[B], op3: FieldDefinition[C], op4: FieldDefinition[D], op5: FieldDefinition[E],
                                     op6: FieldDefinition[F], op7: FieldDefinition[G], op8: FieldDefinition[H]) =
      HList8(op1, op2, op3, op4, op5, op6, op7, op8)

    def obj9[A, B, C, D, E, F, G, H, I](op1: FieldDefinition[A], op2: FieldDefinition[B], op3: FieldDefinition[C], op4: FieldDefinition[D], op5: FieldDefinition[E],
                                        op6: FieldDefinition[F], op7: FieldDefinition[G], op8: FieldDefinition[H], op9: FieldDefinition[I]) =
      HList9(op1, op2, op3, op4, op5, op6, op7, op8, op9)

    def obj10[A, B, C, D, E, F, G, H, I, J](op1: FieldDefinition[A], op2: FieldDefinition[B], op3: FieldDefinition[C], op4: FieldDefinition[D], op5: FieldDefinition[E],
                                            op6: FieldDefinition[F], op7: FieldDefinition[G], op8: FieldDefinition[H], op9: FieldDefinition[I], op10: FieldDefinition[J]) =
      HList10(op1, op2, op3, op4, op5, op6, op7, op8, op9, op10)

    def obj11[A, B, C, D, E, F, G, H, I, J, K](op1: FieldDefinition[A], op2: FieldDefinition[B], op3: FieldDefinition[C], op4: FieldDefinition[D], op5: FieldDefinition[E],
                                               op6: FieldDefinition[F], op7: FieldDefinition[G], op8: FieldDefinition[H], op9: FieldDefinition[I], op10: FieldDefinition[J],
                                               op11: FieldDefinition[K]) =
      HList11(op1, op2, op3, op4, op5, op6, op7, op8, op9, op10, op11)

    def obj12[A, B, C, D, E, F, G, H, I, J, K, L](op1: FieldDefinition[A], op2: FieldDefinition[B], op3: FieldDefinition[C], op4: FieldDefinition[D], op5: FieldDefinition[E],
                                                  op6: FieldDefinition[F], op7: FieldDefinition[G], op8: FieldDefinition[H], op9: FieldDefinition[I], op10: FieldDefinition[J],
                                                  op11: FieldDefinition[K], op12: FieldDefinition[L]) =
      HList12(op1, op2, op3, op4, op5, op6, op7, op8, op9, op10, op11, op12)



  }
}



