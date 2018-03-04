package com.ot.bones.interpreter

import cats.Applicative
import com.ot.bones.IntDataDefinition.{OptionalInt, RequiredInt}
import com.ot.bones.StringDataDefinition.{RequiredString}
import com.ot.bones.ToHList.{ToHListDataDefinitionOp, ToOptionalHListDataDefinitionOp}
import com.ot.bones.convert.DateConversionInstances.DateConversion
import com.ot.bones.convert.UuidConversionInstances.UuidConversion
import com.ot.bones.transform.{OptionalTransform, Transform}
import com.ot.bones.validation._

object DocInterpreter {
//
//  object Doc {
//    implicit val docApp = new Applicative[Doc] {
//      override def pure[A](x: A) = Doc("")
//      override def ap[A, B](ff: Doc[A => B])(fa: Doc[A]): Doc[B] =
//        Doc[B](s" ${ff.str} { ${fa.str} }")
//    }
//  }
//
//
//  case class Doc[A](str: String)
//
//  val stringConversionInterpreter = new cats.arrow.FunctionK[StringConversionOp, Doc] {
//    override def apply[A](fa: StringConversionOp[A]): Doc[A] = { fa match {
//        case op: DateConversion[A,d,o] => Doc(s"Date with format ${op.dateFormat})}")
//        case op: UuidConversion => Doc(s"Converted to UUID)}")
//        case _ => ???
//      }
//    }
//  }
//
//  class DocInterpreter(stringConversionInterperter: cats.arrow.FunctionK[StringConversionOp, Doc]) extends cats.arrow.FunctionK[DataDefinitionOp, Doc] {
//    def keyDesc(key: Key) = key match {
//      case StringKey(name) => s"with key ${name}"
//      case RootKey => ""
//    }
//
//    def apply[A](fgo: DataDefinitionOp[A]): Doc[A] =
//      fgo match {
//        case op: ToHListDataDefinitionOp[a] => {
//          val members = op.members
//          Doc(s"object with ${members.length} members: " + members.map(apply(_)).mkString("(", ")(", ")"))
//        }
//        case op: ToOptionalHListDataDefinitionOp[a] => {
//          val members = op.members
//          Doc(s"optional object with ${members.length} members: " + members.map(apply(_)).mkString("(", ")(", ")"))
//        }
//        case op: RequiredString => Doc(s"Required String ${keyDesc(op.key)}")
//        case op: OptionalString  => Doc(s"Optional String ${keyDesc(op.key)}")
//        case op: RequiredInt => Doc(s"Required Int ${keyDesc(op.key)}")
//        case op: OptionalInt => Doc(s"Optional Int ${keyDesc(op.key)}")
//        case op: DataConversion[A,op,DataDefinitionOp[A],String] => stringConversionInterperter(op.o)
////        case op: OptionalTransform[a,b] => Doc(s"converted to Class ${op.manifestA.runtimeClass.getSimpleName}")
////        case op: Transform[A,a] => Doc(s"converted to Class ${op.manifestA.runtimeClass.getSimpleName}")
////        case op: Transform[z,a] => {
////          val r = apply(op.op)
////          Doc(s"${r} mapped into class ${op.manifestA.runtimeClass.getSimpleName}")
////        }
//
//
//        case _ => ???
//      }
//  }
//

}
