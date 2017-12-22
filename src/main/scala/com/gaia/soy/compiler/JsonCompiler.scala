package com.gaia.soy.compiler

import cats.Applicative
import com.gaia.soy.Obj.{ObjectFieldGroup, OptionalObjectFieldGroup}
import com.gaia.soy.StringValidation.{OptionalString, RequiredString}
import com.gaia.soy.{FieldGroupOp, JsonProducer, Key, RootKey, StringKey}

object JsonCompiler {
  // a function that takes a JsonProducer as input
  type FromProducer[A] = JsonProducer => A
//  type FromProducer[A] = JsonProducer => Validated[NonEmptyList[ExtractionError], A]

  val defaultCompiler = new cats.arrow.FunctionK[FieldGroupOp, FromProducer] {
    def apply[A](fgo: FieldGroupOp[A]): FromProducer[A] = jsonProducer =>
      fgo match {
        case op => op.extract(jsonProducer)
//        case op: RequiredString => op.extract(jsonProducer)
//        case op: OptionalString  => op.extract(jsonProducer)
//        case op: ObjectFieldGroup[_,_] => op.extract(jsonProducer).asInstanceOf[A]
//        case op: OptionalObjectFieldGroup[_,_] => op.extract(jsonProducer).asInstanceOf[A]
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
        case op: RequiredString => Doc(s"Required String ${keyDesc(op.key)}")
        case op: OptionalString  => Doc(s"Optional String ${keyDesc(op.key)}")
        case op: ObjectFieldGroup[_,_] => Doc(s"Required object ${keyDesc(op.key)}.")
        case op: OptionalObjectFieldGroup[_,_] => Doc(s"Optional object ${keyDesc(op.key)} ")

        case _ => ???
      }
  }
}
