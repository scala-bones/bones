package com.bones.data

import java.time.{LocalDate, LocalDateTime}
import java.util.UUID

import com.bones.validation.ValidationDefinition.ValidationOp
import shapeless.ops.hlist.Tupler
import shapeless.{Coproduct, Generic, HList, Nat}

/** KvpValue is meant to be the 'value' of a key value pair.
  * This can be one of the pre-defined primitive 'Bones' types or a product type (to be implemented),
  * one of KvpHList, SumType or
  */
sealed abstract class KvpValue[A: Manifest] {
  val manifestOfA: Manifest[A] = manifest[A]
}

object KvpValue {
  type Path = List[String]
}

/** Wraps a data definition to mark the field optional */
case class OptionalKvpValueDefinition[B: Manifest](valueDefinitionOp: KvpValue[B])
  extends KvpValue[Option[B]] {}

/** Syntactic sugar to wrap the data definition to allow 'optional' syntax on a KvpValue. */
trait ToOptionalData[B] { self: KvpValue[B] =>
  private implicit val manifestOfB: Manifest[B] = self.manifestOfA
  val optional: OptionalKvpValueDefinition[B] = OptionalKvpValueDefinition[B](self)
}

/** Syntactic sugar to wrap the definition in a List type. */
trait ToListData[B] { self: KvpValue[B] =>
  private implicit val manifestOfB: Manifest[B] = self.manifestOfA
  val list: ListData[B] = ListData[B](self, List.empty)
}

/** Schema type for Boolean Data */
final case class BooleanData(validations: List[ValidationOp[Boolean]])
  extends KvpValue[Boolean]
    with ToOptionalData[Boolean]

final case class EitherData[A: Manifest, B: Manifest](definitionA: KvpValue[A],
                                                      definitionB: KvpValue[B])
  extends KvpValue[Either[A, B]]
    with ToOptionalData[Either[A, B]] {}

final case class IntData(validations: List[ValidationOp[Int]])
  extends KvpValue[Int]
    with ToOptionalData[Int]

final case class LongData(validations: List[ValidationOp[Long]])
  extends KvpValue[Long]
    with ToOptionalData[Long]

final case class ListData[T: Manifest](tDefinition: KvpValue[T],
                                       validations: List[ValidationOp[List[T]]])
  extends KvpValue[List[T]]
    with ToOptionalData[List[T]]

final case class ShortData(validations: List[ValidationOp[Short]])
  extends KvpValue[Short]
    with ToOptionalData[Short]

final case class StringData(validations: List[ValidationOp[String]])
  extends KvpValue[String]
    with ToOptionalData[String]

final case class FloatData(validations: List[ValidationOp[Float]])
  extends KvpValue[Float]
    with ToOptionalData[Float]

final case class DoubleData(validations: List[ValidationOp[Double]])
  extends KvpValue[Double]
    with ToOptionalData[Double]

final case class BigDecimalData(validations: List[ValidationOp[BigDecimal]])
  extends KvpValue[BigDecimal]
    with ToOptionalData[BigDecimal]

/** base64-encoded characters, for example,
  * @example "U3dhZ2dlciByb2Nrcw=="
  * */
final case class ByteArrayData(validations: List[ValidationOp[Array[Byte]]])
  extends KvpValue[Array[Byte]]
    with ToOptionalData[Array[Byte]]

final case class LocalDateTimeData(validations: List[ValidationOp[LocalDateTime]])
  extends KvpValue[LocalDateTime]
    with ToOptionalData[LocalDateTime]

final case class LocalDateData(validations: List[ValidationOp[LocalDate]])
  extends KvpValue[LocalDate]
    with ToOptionalData[LocalDate]

final case class UuidData(validations: List[ValidationOp[UUID]])
  extends KvpValue[UUID]
    with ToOptionalData[UUID]

final case class EnumerationData[E<: Enumeration, V:Manifest](enumeration: E,
  validations: List[ValidationOp[V]])
  extends KvpValue[V]
    with ToOptionalData[V] {}

/** Represents a type where the value is an HList */
final case class KvpHListValue[H <: HList : Manifest, HL <: Nat](kvpHList: KvpHList[H, HL],
                                                                 validations: List[ValidationOp[H]])
  extends KvpValue[H]
    with ToOptionalData[H] {

  def convert[Z: Manifest](convertValidation: ValidationOp[Z]*)(
    implicit gen: Generic.Aux[Z, H]): HListConvert[H, HL, Z] =
    HListConvert(kvpHList, gen.from, gen.to, convertValidation.toList)

  def tupled[Tup<:Product:Manifest](tupleValidations: ValidationOp[Tup]*)(
    implicit tupler: Tupler.Aux[H,Tup],
    gen: Generic[Tup]
  ): HListConvert[H,HL,Tup] =
    HListConvert[H,HL,Tup](kvpHList, (h: H) => tupler.apply(h), (t: Tup) => gen.to(t).asInstanceOf[H], tupleValidations.toList)

}

trait BonesSchema[A] {
  val manifestOfA: Manifest[A]
}

/** Represents a coproduct value where the resulting type is a shapeless coproduct */
final case class KvpCoproductValue[C <: Coproduct: Manifest](kvpCoproduct: KvpCoproduct[C])
  extends KvpValue[C]
    with ToOptionalData[C]
    with ToListData[C]
{}

/** Specifies a conversion to and from an HList to an A (where A is most likely a Case class) */
final case class HListConvert[H <: HList, N <: Nat, A: Manifest]
(from: KvpHList[H, N],
 fHtoA: H => A,
 fAtoH: A => H,
 validations: List[ValidationOp[A]]
)
  extends KvpValue[A]
    with ToOptionalData[A]
    with ToListData[A]
    with BonesSchema[A] {}

/** Represents a coproduct value which is to be converted to a class */
final case class KvpCoproductConvert[C<:Coproduct, A: Manifest]
(
  from: KvpCoproduct[C],
  cToA: C => A,
  aToC: A => C,
  validations: List[ValidationOp[A]]
) extends KvpValue[A]
  with ToOptionalData[A]
  with ToListData[A]
  with BonesSchema[A]

