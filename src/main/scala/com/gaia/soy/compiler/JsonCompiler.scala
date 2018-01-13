package com.gaia.soy.compiler

import cats.Applicative
import com.gaia.soy.Obj._
import com.gaia.soy.StringValidation.{OptionalString, RequiredString}
import com.gaia.soy.{FieldGroupOp, JsonProducer, Key, RootKey, StringKey, Transform}
import cats.implicits._
import shapeless._


object JsonCompiler {
  // a function that takes a JsonProducer as input
  type FromProducer[A] = JsonProducer => A
//  type FromProducer[A] = JsonProducer => Validated[NonEmptyList[ExtractionError], A]

  /** Compiler responsible for extracting data from JSON */
  case class DefaultExtractCompiler() extends cats.arrow.FunctionK[FieldGroupOp, FromProducer] {
    def apply[A](fgo: FieldGroupOp[A]): FromProducer[A] = jsonProducer =>
      fgo match {
        case op: Obj2[a,b] => {
          val r1 = apply(op.op1)
          val r2 = apply(op.op2)
          (r1(jsonProducer), r2(jsonProducer)).mapN( _ :: _ :: HNil )
        }
        case op: Obj3[a,b,c] => {
          op.extract(this).apply(jsonProducer)
        }
        case op: RequiredString => op.extract(jsonProducer)
        case op: OptionalString  => op.extract(jsonProducer)
        case op: ObjectFieldGroup[_,_] => op.extract(jsonProducer).asInstanceOf[A]
        case op: OptionalObjectFieldGroup[_,_] => op.extract(jsonProducer).asInstanceOf[A]
        case op: Transform[z,a] => op.extract(this)(jsonProducer).asInstanceOf[A]
//
//        case _ => ???
      }
  }


  object Doc {
    implicit val docApp = new Applicative[Doc] {
      override def pure[A](x: A) = Doc("")
      override def ap[A, B](ff: Doc[A => B])(fa: Doc[A]): Doc[B] =
        Doc[B](s" ${ff.str} { ${fa.str} }")
    }
  }
  case class Doc[A](str: String)

  val docCompiler = new cats.arrow.FunctionK[FieldGroupOp, Doc] {
    def keyDesc(key: Key) = key match {
      case StringKey(name) => s"with key ${name}"
      case RootKey => ""
    }

    def apply[A](fgo: FieldGroupOp[A]): Doc[A] =
      fgo match {
        case op: Obj2[a,b] => {
          val r1 = apply(op.op1)
          val r2 = apply(op.op2)
          Doc(s"object with 2 values (${r1.str}) and (${r2.str})")
        }
        case op: Obj3[a,b,c] => {
          val r1 = apply(op.op1)
          val r2 = apply(op.op2)
          val r3 = apply(op.op3)
          Doc(s"object with 3 values (${r1.str}), (${r2.str}) and (${r3.str}")
        }
        case op: Transform[z,a] => {
          val r = apply(op.op)
          Doc(s"${r} mapped into class ${op.manifestA.runtimeClass.getSimpleName}")
        }
        case op: RequiredString => Doc(s"Required String ${keyDesc(op.key)}")
        case op: OptionalString  => Doc(s"Optional String ${keyDesc(op.key)}")
        case op: ObjectFieldGroup[_,_] => Doc(s"Required object ${keyDesc(op.key)}.")
        case op: OptionalObjectFieldGroup[_,_] => Doc(s"Optional object ${keyDesc(op.key)} ")

        case _ => ???
      }
  }
}
