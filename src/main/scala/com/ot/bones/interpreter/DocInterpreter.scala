package com.ot.bones.interpreter

import cats.Applicative
import com.ot.bones.data.Algebra._
import com.ot.bones.data.HListAlgebra.BaseHListDef

object DocInterpreter {

  object Doc {
    implicit val docApp = new Applicative[Doc] {
      override def pure[A](x: A) = Doc("")
      override def ap[A, B](ff: Doc[A => B])(fa: Doc[A]): Doc[B] =
        Doc[B](s" ${ff.str} { ${fa.str} }")
    }
  }


  case class Doc[A](str: String)

  object DocInterpreter extends cats.arrow.FunctionK[DataDefinitionOp, Doc] {
    def apply[A](fgo: DataDefinitionOp[A]): Doc[A] =
      fgo match {
        case OptionalDataDefinition(dataDefinitionOp) => {
          val desc = apply(dataDefinitionOp)
          Doc(s"Optional: ${desc.str}")
        }
        case op: BaseHListDef[a] => {
          val members = op.members
          Doc(s"object with ${members.length} members: " + members.map(fd => s"${fd.key.name}: ${apply(fd.op).str}").mkString("[", ",", "]"))
        }
        case StringData () => Doc (s"String")
        case IntData () => Doc (s"Int")

        case DoubleData() => Doc(s"Double")
        case BooleanData() => Doc(s"Boolean")
        case ConversionData(op, _, _, desc) => Doc(s"Convert to a $desc from ${apply(op).str}")
        case t: Transform[a,b] => Doc(s"Transform to a ${t.manifestOfA.runtimeClass.getSimpleName} from ${apply(t.op).str}")
        case EitherData(d1, d2) => {
          Doc(s"Either ${apply(d1).str} or ${apply(d2).str}")
        }
        case BigDecimalFromString() => Doc("String representing a BigDecimal")
        case UuidData() => Doc("String representing a UUID")
        case EnumeratedStringData(enum) => Doc(s"String with one of the following values: ${enum.values.map(_.toString).mkString("[",",","]")}")
        case DateData(_, desc) => Doc(s"Date with format $desc")
        case ListData(op) => Doc(s"Array of type ${apply(op).str}")
      }
  }


}
