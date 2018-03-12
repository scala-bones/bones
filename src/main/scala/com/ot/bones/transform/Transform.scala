package com.ot.bones.transform

import cats.arrow.FunctionK
import com.ot.bones.data.Algebra.DataDefinitionOp
import com.ot.bones.data.ToHList.ToHListDataDefinitionOp
import com.ot.bones.interpreter.ExtractionInterpreter.{JsonProducer, ValidateFromProducer}
import shapeless._

/**
  * This class was influenced by scode.Transform.
  * */
case class Transform[A:Manifest,B](op: DataDefinitionOp[B], f: A => B, g: B => A) {

  val manifestA: Manifest[A] = manifest[A]

  def extract[T](functionK: FunctionK[DataDefinitionOp, ValidateFromProducer]): ValidateFromProducer[A] = {
    val fromProducer = functionK.apply(op)
    (jsonProducer: JsonProducer) => {
      fromProducer.apply(jsonProducer).map(res => g.apply(res))
    }
  }

}

case class OptionalTransform[A: Manifest, B](op: DataDefinitionOp[Option[B]], f: A => B, g: B => A) {
  val manifestA: Manifest[A] = manifest[A]

  def extract[T](functionK: FunctionK[DataDefinitionOp, ValidateFromProducer]): ValidateFromProducer[Option[A]] = {
    val bonesProducer = functionK.apply(op)
    (jsonProducer: JsonProducer) => {
      bonesProducer.apply(jsonProducer).map(_.map(g.apply))
    }
  }

}

object Transform {

  def id[A:Manifest](op: DataDefinitionOp[A]) : Transform[A,A] = Transform[A,A](op, identity, identity)

  def fromGeneric[A:Manifest,B](op: DataDefinitionOp[B], gen: Generic.Aux[A, B]): Transform[A, B] =
    Transform(op, gen.to _, gen.from _)

  def fromGenericOptional[A:Manifest,B](op: DataDefinitionOp[Option[B]], gen: Generic.Aux[A, B]): OptionalTransform[A, B] =
    OptionalTransform[A,B](op, gen.to _, gen.from _)

}

trait TransformSyntax {

  implicit class HListToTransform[L <: HList](op: ToHListDataDefinitionOp[L]) {
    def transform[Z:Manifest](implicit gen: Generic.Aux[Z, L]) : Transform[Z, L]
      = Transform.fromGeneric(op, gen)
  }


}
