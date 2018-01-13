package com.ot.bones.transform

import cats.arrow.FunctionK
import cats.data.Validated.Valid
import com.ot.bones.BonesOp
import com.ot.bones.compiler.ExtractionCompiler.{FromProducer, JsonProducer}
import com.ot.bones.validation.ToHList.{ToHListBonesOp, ToOptionalHListBonesOp}
import shapeless._

/**
  * This class was influenced by scode.Transform.
  * */
case class Transform[A:Manifest,B](op: BonesOp[B], f: A => B, g: B => A) extends BonesOp[A] {

  val manifestA: Manifest[A] = manifest[A]

  def extract[T](functionK: FunctionK[BonesOp, FromProducer]): FromProducer[A] = {
    val fromProducer = functionK.apply(op)
    (jsonProducer: JsonProducer) => {
      fromProducer.apply(jsonProducer).map(res => g.apply(res))
    }
  }

}

case class OptionalTransform[A: Manifest, B](op: BonesOp[Option[B]], f: A => B, g: B => A) extends BonesOp[Option[A]] {
  val manifestA: Manifest[A] = manifest[A]

  def extract[T](functionK: FunctionK[BonesOp, FromProducer]): FromProducer[Option[A]] = {
    val bonesProducer = functionK.apply(op)
    (jsonProducer: JsonProducer) => {
      bonesProducer.apply(jsonProducer).map(_.map(g.apply))
    }
  }

}

object Transform {

  def id[A:Manifest](op: BonesOp[A]) : Transform[A,A] = Transform[A,A](op, identity, identity)

  def fromGeneric[A:Manifest,B](op: BonesOp[B], gen: Generic.Aux[A, B]): Transform[A, B] =
    Transform(op, gen.to _, gen.from _)

  def fromGenericOptional[A:Manifest,B](op: BonesOp[Option[B]], gen: Generic.Aux[A, B]): OptionalTransform[A, B] =
    OptionalTransform[A,B](op, gen.to _, gen.from _)

}

trait TransformSyntax {

  implicit class HListToTransform[L <: HList](op: ToHListBonesOp[L]) {
    def transform[Z:Manifest](implicit gen: Generic.Aux[Z, L]) : Transform[Z, L]
      = Transform.fromGeneric(op, gen)
  }

  implicit class OptionalHListToTransform[L <: HList](op: ToOptionalHListBonesOp[L]) {
    def transform[Z:Manifest](implicit gen: Generic.Aux[Z, L]) : OptionalTransform[Z, L]
      = Transform.fromGenericOptional(op, gen)
  }

}
