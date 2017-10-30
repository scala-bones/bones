package com.gaia.soy

import cats.{Applicative, Apply, Id}
import cats.data.{NonEmptyList, Validated}
import cats.implicits._
import cats.free.FreeApplicative.lift
import com.gaia.soy.StringValidation.{OptionalString, RequiredString}

object Soyo {

  type FG[A] = FieldGroup[ValidationResultNel[A]]
  type FGO[A] = FieldGroupOp[ValidationResultNel[A]]

//  case class Obj4[A,B,C,D,E](op1: FGO[A], op2: FGO[B], op3: FGO[C], op4: FGO[D]) extends FGO[(A,B,C,D)] {
//
//    def apply(f: (A,B,C,D) => E): FG[E] = {
//      (
//        lift(op1).asInstanceOf[FieldGroup[ValidationResultNel[A]]],
//        lift(op2).asInstanceOf[FieldGroup[ValidationResultNel[B]]],
//        lift(op3).asInstanceOf[FieldGroup[ValidationResultNel[C]]],
//        lift(op4).asInstanceOf[FieldGroup[ValidationResultNel[D]]]
//      ).mapN { case (v1,v2,v3,v4) =>  {
//        Applicative[Validated[NonEmptyList[ExtractionError], ?]].map4(v1,v2,v3,v4)(f(_, _, _, _))
//      }}
//    }
//  }


  def obj4[A,B,C,D,E](op1: FGO[A],
                  op2: FGO[B],
                  op3: FGO[C],
                  op4: FGO[D],
                  f: (A,B,C,D) => E) : FG[E] = {
    (
      lift(op1).asInstanceOf[FieldGroup[ValidationResultNel[A]]],
      lift(op2).asInstanceOf[FieldGroup[ValidationResultNel[B]]],
      lift(op3).asInstanceOf[FieldGroup[ValidationResultNel[C]]],
      lift(op4).asInstanceOf[FieldGroup[ValidationResultNel[D]]]
    ).mapN { case (v1,v2,v3,v4) =>  {
      Applicative[Validated[NonEmptyList[ExtractionError], ?]].map4(v1,v2,v3,v4)(f(_, _, _, _))
    }}
  }


//  case class Ap2[A,B,F[_],G[_]](e1: Extraction[F,A], e2: Extraction[G,B]) extends Extraction[Id,(F[A],G[B])] {
//    override type I = String
//
//    override def extract(stringProducer: StringProducer): Either[ExtractionErrors, (F[A], G[B])] = {
//
//      val r1 = e1.extract(stringProducer)
//      val r2 = e2.extract(stringProducer)
//      Apply[Either[ExtractionErrors,?]].map2(r1,r2)( (a,b) => (a,b))
//
//    }
//  }
//
//
//  case class Ap4[A,B,C,D,FA[_],FB[_],FC[_],FD[_]](
//    e1: Extraction[FA,A], e2: Extraction[FB,B], e3: Extraction[FC,C], e4: Extraction[FD,D]
//  ) extends Extraction[Id,(FA[A], FB[B], FC[C], FD[D])] {
//    override type I = this.type
//
//    override def extract(stringProducer: RawValueProducer): Either[ExtractionErrors, (FA[A], FB[B], FC[C], FD[D])] = {
//      val r1 = e1.extract(stringProducer)
//      val r2 = e2.extract(stringProducer)
//      val r3 = e3.extract(stringProducer)
//      val r4 = e4.extract(stringProducer)
//      Apply[Either[ExtractionErrors, ?]].map4(r1, r2, r3, r4)((a, b, c, d) => (a, b, c, d))
//    }
//  }

}

