package com.gaia.soy

import cats.arrow.FunctionK
import com.gaia.soy.compiler.JsonCompiler.FromProducer
import shapeless._

/**
  * This class was influenced by scode.Transform.
  * */
case class Transform[A:Manifest,B](op: FieldGroupOp[ValidationResultNel[B]], f: A => B, g: B => A) extends FieldGroupOp[ValidationResultNel[A]] {

  val manifestA: Manifest[A] = manifest[A]

  def extract[T](functionK: FunctionK[FieldGroupOp, FromProducer]): FromProducer[ValidationResultNel[A]] = {
    val tuples = functionK.apply(op)
    (jsonProducer: JsonProducer) => {
      tuples.apply(jsonProducer).map(res => g.apply(res))
    }
  }


}

object Transform {

  def id[A:Manifest](op: FieldGroupOp[ValidationResultNel[A]]) : Transform[A,A] = Transform[A,A](op, identity, identity)

  def fromGeneric[A:Manifest,B](op: FieldGroupOp[ValidationResultNel[B]], gen: Generic.Aux[A, B]): Transform[A, B] =
    Transform(op, gen.to _, gen.from _)


}
