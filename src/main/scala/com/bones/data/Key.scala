package com.bones.data

import java.text.{DateFormat, SimpleDateFormat}
import java.util.{Date, UUID}

import com.bones.data.Algebra._
import com.bones.data.HListAlgebra._
import com.bones.interpreter.ExtractionInterpreter.CanNotConvert
import com.bones.validation.ValidationDefinition.ListValidation.PassesAll
import com.bones.validation.ValidationDefinition.{ToOptionalValidation, ValidationOp}
import shapeless.{::, Generic, HNil}

trait KeySyntax {
  def key(key: String) = Key(key)

  /** Implicit to turn a string a key. */
  implicit class StringToKey(str: String) {
    def key(): Key = Key(str)
  }
}

/**
  * A field definition is essentially a key value pair and a list of validations to be applied to the value.
  * @tparam A The type this field definition is describing.
  */
trait FieldDefinition[A] {
  /** String key, aka field name */
  val key: Key
  /** One of the Data Definitions describing the data of this field. */
  val op: DataDefinitionOp[A]
  /** List of validations this field should adhere to*/
  val validations: List[ValidationOp[A]]

  val asMember: HMember[A] = HMember(this, List.empty)
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

  def convert[B](fab: A => Either[CanNotConvert[A,B], B], fba: B => A, description: String, validations: List[ValidationOp[B] with ToOptionalValidation[B]]): ConversionFieldDefinition[A,B] = {
    val cd = ConversionData[A,B](op, fab, fba, description)
    ConversionFieldDefinition[A,B](this, cd, validations)
  }

  def transform[Z:Manifest]()(implicit gen: Generic.Aux[Z, A]): ConversionFieldDefinition[A,Z] = {
    val newOp = ConversionData(op, (a: A) => Right(gen.from(a)), gen.to, s"Transform to type ${manifest[Z].runtimeClass.getSimpleName}")
    ConversionFieldDefinition(this, newOp, List.empty)
  }

}

case class ConversionFieldDefinition[A,B](convertFrom: RequiredFieldDefinition[A], op: ConversionData[A,B], validations: List[ValidationOp[B] with ToOptionalValidation[B]])
  extends FieldDefinition[B] {
  /** String key, aka field name */
  override val key: Key = convertFrom.key

  def optional(): OptionalFieldDefinition[Option[B]] = {
    val optionalValidations = validations.map(_.toOption)
    OptionalFieldDefinition(key, op.toOption, optionalValidations)
  }
}


/** Starting point for obtaining a value is to define a key */
case class Key(name: String) { thisKey =>
  val key: Key = thisKey

  /** Indicates that the data tied to this key is a String type that must pass the specified validations */
  def string(f: ValidationOp[String] with ToOptionalValidation[String] *): RequiredFieldDefinition[String] = RequiredFieldDefinition(this, StringData(), f.toList)

  /** Indicates that the data tied to this key is an Int type that must pass the specified validations */
  def int(f: ValidationOp[Int] with ToOptionalValidation[Int] *): RequiredFieldDefinition[Int] = RequiredFieldDefinition[Int](this, IntData(), f.toList)

  /** Indicates that the data tied to this key is an Double type that must pass the specified validations.
    * In the JSON world, this would be a number which is converted to a Double.
    **/
  def double(f: ValidationOp[Double] with ToOptionalValidation[Double] *): RequiredFieldDefinition[Double] =
    RequiredFieldDefinition[Double](this, DoubleData(), List.empty)

  /**
    * Indicates that the data tied to this key is a list (JSON Array) type.  All values are type
    * T and all values must pass the list of validations.
    *
    * @param dataDefinitionOp - One of the supported DataDefinitionOp types.
    * @param v List of validations each element of the list must pass to be valid.
    * @tparam T The type of each element.  Can be an EitherFieldDefinition if more than one type is expected in the list.
    * @tparam L The List[T] type.
    * @return
    */
  def list[T, L <: List[T]](dataDefinitionOp: DataDefinitionOp[T], v: ValidationOp[T] with ToOptionalValidation[T]*): RequiredFieldDefinition[L] =
    RequiredFieldDefinition[L](this, ListData(dataDefinitionOp), List(PassesAll[T,L](v.toList)))


  /** Indicates that the data tied to this key is an boolean type that must pass the specified validations. */
  def boolean(f: ValidationOp[Boolean] with ToOptionalValidation[Boolean] *): RequiredFieldDefinition[Boolean] =
    RequiredFieldDefinition[Boolean](this, BooleanData(), f.toList)

  /** Indicates that the data tied to this key is a UUID type that must pass the specified validations. */
  def uuid(v: ValidationOp[UUID] with ToOptionalValidation[UUID] *): RequiredFieldDefinition[UUID] = RequiredFieldDefinition[UUID](this, UuidData(), v.toList)

  /** Indicates that the data tied to this key is a Date type with the specified format that must pass the specified validations. */
  def date(dateFormat: DateFormat, formatDescription: String, v: ValidationOp[Date] with ToOptionalValidation[Date]*): RequiredFieldDefinition[Date] =
    RequiredFieldDefinition(key, DateData(dateFormat, formatDescription), v.toList)

  /** Indicates that the data tied to this key is a BigDecimal that must pass the specified validations. */
  def bigDecimal(v: ValidationOp[BigDecimal] with ToOptionalValidation[BigDecimal]*): RequiredFieldDefinition[BigDecimal] =
    RequiredFieldDefinition[BigDecimal](key, BigDecimalFromString(), v.toList)

  /** Indicates that the data tied to this key is a Date type with the specified format that must pass the specified validations. */
  def either[A,B](definitionA: DataDefinitionOp[A], definitionB: DataDefinitionOp[B]) : RequiredFieldDefinition[Either[A,B]] =
    RequiredFieldDefinition[Either[A,B]](key, EitherData(definitionA, definitionB), List.empty)

  /** Expecting a string that is in the format of an iso date time */
  def isoDateTime(v: ValidationOp[Date] with ToOptionalValidation[Date]*): RequiredFieldDefinition[Date] =
    RequiredFieldDefinition(key,
      DateData(
        new SimpleDateFormat("yyyy-MM-dd'T'HH:mm'Z'"),
        "ISO date-time format with the offset and zone if available, such as '2011-12-03T10:15:30', '2011-12-03T10:15:30+01:00' or '2011-12-03T10:15:30+01:00[Europe/Paris]'"
      ),
      v.toList
    )


  /** Expecting a string that is in the format of an iso date */
  def isoDate(v: ValidationOp[Date] with ToOptionalValidation[Date]*): RequiredFieldDefinition[Date] = RequiredFieldDefinition(key, isoDateData, v.toList)
  private val isoDateData = DateData(
    new SimpleDateFormat("yyyy-MM-dd"),
    "ISO date format with the offset if available, such as '2011-12-03' or '2011-12-03+01:00'"
  )

  def enumeration(e: Enumeration): RequiredFieldDefinition[e.Value] =
    RequiredFieldDefinition(key, EnumeratedStringData(e), List.empty)

  def enumeration(e: Enumeration)(v: ValidationOp[e.Value] with ToOptionalValidation[e.Value]*): RequiredFieldDefinition[e.Value] =
    RequiredFieldDefinition(key, EnumeratedStringData(e), v.toList)

  def obj1[A](op1: FieldDefinition[A]): RequiredFieldDefinition[A :: HNil] =
    RequiredFieldDefinition[A :: HNil](this, HMember(op1, List.empty), List.empty)

  def obj2[A, B](op1: FieldDefinition[A], op2: FieldDefinition[B]): RequiredFieldDefinition[A :: B :: HNil] =
    RequiredFieldDefinition[A :: B :: HNil](this, op1.asMember :: op2.asMember , List.empty)

  def obj3[A, B, C](op1: FieldDefinition[A], op2: FieldDefinition[B], op3: FieldDefinition[C]): RequiredFieldDefinition[A :: B :: C :: HNil] =
    RequiredFieldDefinition[A :: B :: C :: HNil](this, op1.asMember :: op2.asMember :: op3.asMember, List.empty)

  def obj4[A, B, C, D](op1: FieldDefinition[A], op2: FieldDefinition[B], op3: FieldDefinition[C], op4: FieldDefinition[D]):
    RequiredFieldDefinition[A :: B :: C :: D :: HNil] =
    RequiredFieldDefinition[A :: B :: C :: D :: HNil](this, op1.asMember :: op2.asMember :: op3.asMember :: op4.asMember, List.empty)

  def obj5[A, B, C, D, E](op1: FieldDefinition[A], op2: FieldDefinition[B], op3: FieldDefinition[C], op4: FieldDefinition[D],
                          op5: FieldDefinition[E]):
  RequiredFieldDefinition[A :: B :: C :: D :: E :: HNil] =
    RequiredFieldDefinition[A :: B :: C :: D :: E :: HNil](this,  op1.asMember :: op2.asMember :: op3.asMember :: op4.asMember :: op5.asMember, List.empty)

  def obj6[A, B, C, D, E, F](op1: FieldDefinition[A], op2: FieldDefinition[B], op3: FieldDefinition[C], op4: FieldDefinition[D],
                             op5: FieldDefinition[E], op6: FieldDefinition[F]):
  RequiredFieldDefinition[A :: B :: C :: D :: E :: F :: HNil] =
    RequiredFieldDefinition[A :: B :: C :: D :: E :: F :: HNil](this, op1.asMember :: op2.asMember :: op3.asMember :: op4.asMember :: op5.asMember :: op6.asMember, List.empty)

  def obj7[A, B, C, D, E, F, G](op1: FieldDefinition[A], op2: FieldDefinition[B], op3: FieldDefinition[C], op4: FieldDefinition[D],
                                op5: FieldDefinition[E], op6: FieldDefinition[F], op7: FieldDefinition[G]):
  RequiredFieldDefinition[A :: B :: C :: D :: E :: F :: G :: HNil] =
    RequiredFieldDefinition[A :: B :: C :: D :: E :: F :: G :: HNil](this, op1.asMember :: op2.asMember :: op3.asMember :: op4.asMember :: op5.asMember :: op6.asMember :: op7.asMember, List.empty)

  def obj8[A, B, C, D, E, F, G, H](op1: FieldDefinition[A], op2: FieldDefinition[B], op3: FieldDefinition[C], op4: FieldDefinition[D],
                                op5: FieldDefinition[E], op6: FieldDefinition[F], op7: FieldDefinition[G], op8: FieldDefinition[H]):
  RequiredFieldDefinition[A :: B :: C :: D :: E :: F :: G :: H :: HNil] =
    RequiredFieldDefinition[A :: B :: C :: D :: E :: F :: G :: H :: HNil](this, op1.asMember :: op2.asMember :: op3.asMember :: op4.asMember :: op5.asMember :: op6.asMember :: op7.asMember :: op8.asMember, List.empty)

  def obj9[A, B, C, D, E, F, G, H, I](op1: FieldDefinition[A], op2: FieldDefinition[B], op3: FieldDefinition[C], op4: FieldDefinition[D],
                                      op5: FieldDefinition[E], op6: FieldDefinition[F], op7: FieldDefinition[G], op8: FieldDefinition[H],
                                      op9: FieldDefinition[I]):
  RequiredFieldDefinition[A :: B :: C :: D :: E :: F :: G :: H :: I :: HNil] =
    RequiredFieldDefinition[A :: B :: C :: D :: E :: F :: G :: H :: I :: HNil](this, op1.asMember :: op2.asMember :: op3.asMember :: op4.asMember :: op5.asMember :: op6.asMember :: op7.asMember :: op8.asMember :: op9.asMember, List.empty)

  def obj10[A, B, C, D, E, F, G, H, I, J](op1: FieldDefinition[A], op2: FieldDefinition[B], op3: FieldDefinition[C], op4: FieldDefinition[D],
                                        op5: FieldDefinition[E], op6: FieldDefinition[F], op7: FieldDefinition[G], op8: FieldDefinition[H],
                                        op9: FieldDefinition[I], op10: FieldDefinition[J]):
  RequiredFieldDefinition[A :: B :: C :: D :: E :: F :: G :: H :: I :: J :: HNil] =
    RequiredFieldDefinition[A :: B :: C :: D :: E :: F :: G :: H :: I :: J :: HNil](this,  op1.asMember :: op2.asMember :: op3.asMember :: op4.asMember :: op5.asMember :: op6.asMember :: op7.asMember :: op8.asMember :: op9.asMember :: op10.asMember, List.empty)

  def obj11[A, B, C, D, E, F, G, H, I, J, K](op1: FieldDefinition[A], op2: FieldDefinition[B], op3: FieldDefinition[C], op4: FieldDefinition[D],
                                             op5: FieldDefinition[E], op6: FieldDefinition[F], op7: FieldDefinition[G], op8: FieldDefinition[H],
                                             op9: FieldDefinition[I], op10: FieldDefinition[J], op11: FieldDefinition[K]):
  RequiredFieldDefinition[A :: B :: C :: D :: E :: F :: G :: H :: I :: J :: K :: HNil] =
    RequiredFieldDefinition[A :: B :: C :: D :: E :: F :: G :: H :: I :: J :: K :: HNil](
      this, op1.asMember :: op2.asMember :: op3.asMember :: op4.asMember :: op5.asMember :: op6.asMember :: op7.asMember :: op8.asMember :: op9.asMember :: op10.asMember :: op11.asMember, List.empty)


  def obj12[A, B, C, D, E, F, G, H, I, J, K, L](op1: FieldDefinition[A], op2: FieldDefinition[B], op3: FieldDefinition[C], op4: FieldDefinition[D],
                                             op5: FieldDefinition[E], op6: FieldDefinition[F], op7: FieldDefinition[G], op8: FieldDefinition[H],
                                             op9: FieldDefinition[I], op10: FieldDefinition[J], op11: FieldDefinition[K], op12: FieldDefinition[L]):
  RequiredFieldDefinition[A :: B :: C :: D :: E :: F :: G :: H :: I :: J :: K :: L :: HNil] =
    RequiredFieldDefinition[A :: B :: C :: D :: E :: F :: G :: H :: I :: J :: K :: L :: HNil](
      this, op1.asMember :: op2.asMember :: op3.asMember :: op4.asMember :: op5.asMember :: op6.asMember :: op7.asMember :: op8.asMember :: op9.asMember :: op10.asMember :: op11.asMember :: op12.asMember, List.empty
    )

}

object Sugar {
  /** Aliases for creating HList types. */
  trait ToHList {

    def obj1[A, AA](op1: FieldDefinition[A]) = HMember(op1, List.empty)

    def obj2[A, AA, B, BB](op1: FieldDefinition[A],
                           op2: FieldDefinition[B]) = HMember(op1, List.empty) :: obj1(op2)

    def obj3[A, B, C](op1: FieldDefinition[A], op2: FieldDefinition[B], op3: FieldDefinition[C]) =
      HMember(op1, List.empty) :: obj2(op2,op3)

    def obj4[A, B, C, D](op1: FieldDefinition[A], op2: FieldDefinition[B], op3: FieldDefinition[C], op4: FieldDefinition[D]) =
      HMember(op1, List.empty) :: obj3(op2,op3, op4)

    def obj5[A, B, C, D, E](op1: FieldDefinition[A], op2: FieldDefinition[B], op3: FieldDefinition[C], op4: FieldDefinition[D], op5: FieldDefinition[E]) =
      HMember(op1, List.empty) :: obj4(op2,op3, op4, op5)

    def obj6[A, B, C, D, E, F](op1: FieldDefinition[A], op2: FieldDefinition[B], op3: FieldDefinition[C], op4: FieldDefinition[D], op5: FieldDefinition[E],
                               op6: FieldDefinition[F]) =
      HMember(op1, List.empty) :: obj5(op2,op3, op4, op5, op6)

    def obj7[A, B, C, D, E, F, G](op1: FieldDefinition[A], op2: FieldDefinition[B], op3: FieldDefinition[C], op4: FieldDefinition[D], op5: FieldDefinition[E],
                                  op6: FieldDefinition[F], op7: FieldDefinition[G]) =
      HMember(op1, List.empty) :: obj6(op2,op3, op4, op5, op6, op7)

    def obj8[A, B, C, D, E, F, G, H](op1: FieldDefinition[A], op2: FieldDefinition[B], op3: FieldDefinition[C], op4: FieldDefinition[D], op5: FieldDefinition[E],
                                     op6: FieldDefinition[F], op7: FieldDefinition[G], op8: FieldDefinition[H]) =
      HMember(op1, List.empty) :: obj7(op2,op3, op4, op5, op6, op7, op8)

    def obj9[A, B, C, D, E, F, G, H, I](op1: FieldDefinition[A], op2: FieldDefinition[B], op3: FieldDefinition[C], op4: FieldDefinition[D], op5: FieldDefinition[E],
                                        op6: FieldDefinition[F], op7: FieldDefinition[G], op8: FieldDefinition[H], op9: FieldDefinition[I]) =
      HMember(op1, List.empty) :: obj8(op2,op3, op4, op5, op6, op7, op8, op9)

    def obj10[A, B, C, D, E, F, G, H, I, J](op1: FieldDefinition[A], op2: FieldDefinition[B], op3: FieldDefinition[C], op4: FieldDefinition[D], op5: FieldDefinition[E],
                                            op6: FieldDefinition[F], op7: FieldDefinition[G], op8: FieldDefinition[H], op9: FieldDefinition[I], op10: FieldDefinition[J]) =
      HMember(op1, List.empty) :: obj9(op2,op3, op4, op5, op6, op7, op8, op9, op10)

    def obj11[A, B, C, D, E, F, G, H, I, J, K](op1: FieldDefinition[A], op2: FieldDefinition[B], op3: FieldDefinition[C], op4: FieldDefinition[D], op5: FieldDefinition[E],
                                               op6: FieldDefinition[F], op7: FieldDefinition[G], op8: FieldDefinition[H], op9: FieldDefinition[I], op10: FieldDefinition[J],
                                               op11: FieldDefinition[K]) =
      HMember(op1, List.empty) :: obj10(op2,op3, op4, op5, op6, op7, op8, op9, op10, op11)

    def obj12[A, B, C, D, E, F, G, H, I, J, K, L](op1: FieldDefinition[A], op2: FieldDefinition[B], op3: FieldDefinition[C], op4: FieldDefinition[D], op5: FieldDefinition[E],
                                                  op6: FieldDefinition[F], op7: FieldDefinition[G], op8: FieldDefinition[H], op9: FieldDefinition[I], op10: FieldDefinition[J],
                                                  op11: FieldDefinition[K], op12: FieldDefinition[L]) =
      HMember(op1, List.empty) :: obj11(op2,op3, op4, op5, op6, op7, op8, op9, op10, op11, op12)

    def obj13[A, B, C, D, E, F, G, H, I, J, K, L, M](op1: FieldDefinition[A], op2: FieldDefinition[B], op3: FieldDefinition[C], op4: FieldDefinition[D], op5: FieldDefinition[E],
                                                  op6: FieldDefinition[F], op7: FieldDefinition[G], op8: FieldDefinition[H], op9: FieldDefinition[I], op10: FieldDefinition[J],
                                                  op11: FieldDefinition[K], op12: FieldDefinition[L], op13: FieldDefinition[M]) =
      HMember(op1, List.empty) :: obj12(op2,op3, op4, op5, op6, op7, op8, op9, op10, op11, op12, op13)

    def obj14[A, B, C, D, E, F, G, H, I, J, K, L,M, N](op1: FieldDefinition[A], op2: FieldDefinition[B], op3: FieldDefinition[C], op4: FieldDefinition[D], op5: FieldDefinition[E],
                                                    op6: FieldDefinition[F], op7: FieldDefinition[G], op8: FieldDefinition[H], op9: FieldDefinition[I], op10: FieldDefinition[J],
                                                    op11: FieldDefinition[K], op12: FieldDefinition[L], op13: FieldDefinition[M],
                                                    op14: FieldDefinition[N]) =
      HMember(op1, List.empty) :: obj13(op2,op3, op4, op5, op6, op7, op8, op9, op10, op11, op12, op13, op14)


  }
}



