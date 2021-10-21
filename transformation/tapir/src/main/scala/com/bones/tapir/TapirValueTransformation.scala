package com.bones.tapir

import com.bones.data.values.CNilF
import shapeless.{:+:, Coproduct, Inl, Inr}
import sttp.tapir.SchemaType

object TapirValueTransformation {

  /** using kind projector allows us to create a new interpreter by merging two existing
    * interpreters. see https://stackoverflow.com/a/60561575/387094
    */
  def merge[L[_], R[_] <: Coproduct](
    li: TapirValueTransformation[L],
    ri: TapirValueTransformation[R]
  ): TapirValueTransformation[Lambda[V => L[V] :+: R[V]]] =
    new TapirValueTransformation[Lambda[V => L[V] :+: R[V]]] {
      override def toSchemaType[A](
        alg: L[A] :+: R[A],
        description: Option[String],
        example: Option[A]
      ): (SchemaType, DescriptionString, ExampleString) =
        alg match {
          case Inl(l) => li.toSchemaType(l, description, example)
          case Inr(r) => ri.toSchemaType(r, description, example)
        }
    }

  implicit class InterpreterOps[ALG[_], OUT](val base: TapirValueTransformation[ALG])
      extends AnyVal {
    def ++[R[_] <: Coproduct](
      r: TapirValueTransformation[R]
    ): TapirValueTransformation[Lambda[A => ALG[A] :+: R[A]]] =
      merge(base, r)
  }

  case object CNilInterchangeFormatEncoder extends TapirValueTransformation[CNilF] {
    override def toSchemaType[A](
      alg: CNilF[A],
      description: Option[DescriptionString],
      example: Option[A]
    ): (SchemaType, DescriptionString, ExampleString) =
      sys.error("Unreachable code")
  }
}

/** Each Algebra (ALG) is responsible for transforming itself into tapir data
  * @tparam ALG
  */
trait TapirValueTransformation[ALG[_]] {
  def toSchemaType[A](
    alg: ALG[A],
    description: Option[String],
    example: Option[A]
  ): (SchemaType, DescriptionString, ExampleString)
}
