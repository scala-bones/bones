package com.gaia.soy

import cats.Applicative
import cats.data.Validated.Invalid
import cats.data.{NonEmptyList, Validated}
import cats.free.FreeApplicative.lift
import cats.implicits._

object Soyo {

  trait Obj { key: Key =>

    def obj2[A,B,Z](
     op1: FieldGroupOp[ValidationResultNel[A]],
     op2: FieldGroupOp[ValidationResultNel[B]],
     f: (A,B) => Z
   ) = Obj2(key, op1, op2, f)


    def obj4[A,B,C,D,Z](
      op1: FieldGroupOp[ValidationResultNel[A]],
      op2: FieldGroupOp[ValidationResultNel[B]],
      op3: FieldGroupOp[ValidationResultNel[C]],
      op4: FieldGroupOp[ValidationResultNel[D]],
      f: (A,B,C,D) => Z
    ) = Obj4(key, op1, op2, op3, op4,f)
  }

  type FG[A] = FieldGroup[ValidationResultNel[A]]
  type FGO[A] = FieldGroupOp[ValidationResultNel[A]]

  /** Extract method can derived from */
  abstract class ObjectFieldGroup[AA,ZZ] {

    def key: Key
    def extractChildren(jsonProducer: JsonProducer): ValidationResultNel[AA]
    def tupledF: AA => ZZ

    def extract(producer: JsonProducer): Validated[ExtractionErrors, ZZ] = {
      producer.produceObject(key).leftMap(NonEmptyList.one).toValidated.andThen {
        case Some(producer) => extractChildren(producer).map(tupledF)
        case None => Invalid(NonEmptyList.one(RequiredObjectError(key)))
      }
    }

  }

  case class Obj2[A,B,Z](key: Key,
                         op1: FieldGroupOp[ValidationResultNel[A]],
                         op2: FieldGroupOp[ValidationResultNel[B]],
                         f: (A,B) => Z)
    extends ObjectFieldGroup[(A,B),Z] with FieldGroupOp[ValidationResultNel[Z]] {

    def tupledF = f.tupled

    def extractChildren(jsonProducer: JsonProducer): ValidationResultNel[(A,B)] = {
      ( op1.extract(jsonProducer), op2.extract(jsonProducer)).mapN( (_,_) )
    }

  }


  case class Obj4[A,B,C,D,Z](key: Key,
                             op1: FieldGroupOp[ValidationResultNel[A]],
                             op2: FieldGroupOp[ValidationResultNel[B]],
                             op3: FieldGroupOp[ValidationResultNel[C]],
                             op4: FieldGroupOp[ValidationResultNel[D]],
                             f: (A,B,C,D) => Z)
    extends ObjectFieldGroup[(A,B,C,D),Z] with FieldGroupOp[ValidationResultNel[Z]] {

    def tupledF = f.tupled

    def extractChildren(jsonProducer: JsonProducer): ValidationResultNel[(A,B,C,D)] = {
      ( op1.extract(jsonProducer), op2.extract(jsonProducer), op3.extract(jsonProducer), op4.extract(jsonProducer))
          .mapN( (_,_,_,_) )
    }

  }


  def obj4[A,B,C,D,E](op1: FieldGroupOp[ValidationResultNel[A]],
                  op2: FieldGroupOp[ValidationResultNel[B]],
                  op3: FieldGroupOp[ValidationResultNel[C]],
                  op4: FieldGroupOp[ValidationResultNel[D]],
                  f: (A,B,C,D) => E) : FieldGroup[ValidationResultNel[E]] = {
    (
      lift(op1).asInstanceOf[FieldGroup[ValidationResultNel[A]]],
      lift(op2).asInstanceOf[FieldGroup[ValidationResultNel[B]]],
      lift(op3).asInstanceOf[FieldGroup[ValidationResultNel[C]]],
      lift(op4).asInstanceOf[FieldGroup[ValidationResultNel[D]]]
    ).mapN { case (v1,v2,v3,v4) =>  {
      Applicative[Validated[NonEmptyList[ExtractionError], ?]].map4(v1,v2,v3,v4)(f(_, _, _, _))
    }}
  }


}

