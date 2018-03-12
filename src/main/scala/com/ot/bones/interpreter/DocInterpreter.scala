package com.ot.bones.interpreter

import cats.Applicative
import com.ot.bones.data.Algebra.{DataDefinitionOp, IntData, OptionalDataDefinition, StringData}
import com.ot.bones.data.ToHList.ToHListDataDefinitionOp

object DocInterpreter {

  object Doc {
    implicit val docApp = new Applicative[Doc] {
      override def pure[A](x: A) = Doc("")
      override def ap[A, B](ff: Doc[A => B])(fa: Doc[A]): Doc[B] =
        Doc[B](s" ${ff.str} { ${fa.str} }")
    }
  }


  case class Doc[A](str: String)

  class DocInterpreter(stringConversionInterperter: cats.arrow.FunctionK[DataDefinitionOp, Doc]) extends cats.arrow.FunctionK[DataDefinitionOp, Doc] {
    def apply[A](fgo: DataDefinitionOp[A]): Doc[A] =
      fgo match {
        case OptionalDataDefinition(dataDefinitionOp) => {
          val desc = this(dataDefinitionOp)
          Doc(s"Optional ${desc}")
        }
        case op: ToHListDataDefinitionOp[a] => {
          val members = op.members
          Doc(s"object with ${members.length} members: " + members.map(fd => apply(fd.op)).mkString("(", ")(", ")"))
        }
        case StringData () => Doc (s"String")
        case IntData () => Doc (s"Int")
        //        case op: DataConversion[A,op,DataDefinitionOp[A],String] => stringConversionInterperter(op.o)
        //        case op: OptionalTransform[a,b] => Doc(s"converted to Class ${op.manifestA.runtimeClass.getSimpleName}")
        //        case op: Transform[A,a] => Doc(s"converted to Class ${op.manifestA.runtimeClass.getSimpleName}")
        //        case op: Transform[z,a] => {
        //          val r = apply(op.op)
        //          Doc(s"${r} mapped into class ${op.manifestA.runtimeClass.getSimpleName}")
        //        }


        case x => Doc(s"Not Implemented yet")
      }
  }


}
