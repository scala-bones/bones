package com.gaia.soy

import cats.{Apply, Id}
import cats.data.NonEmptyList
import cats.implicits._


object Soyo {


  case class Ap2[A,B,F[_],G[_]](e1: Extraction[F,A], e2: Extraction[G,B]) extends Extraction[Id,(F[A],G[B])] {
    override type I = String

    override def extract(stringProducer: RawValueProducer): Either[ExtractionErrors, (F[A], G[B])] = {

      val r1 = e1.extract(stringProducer)
      val r2 = e2.extract(stringProducer)
      Apply[Either[ExtractionErrors,?]].map2(r1,r2)( (a,b) => (a,b))

    }
  }


  case class Ap4[A,B,C,D,FA[_],FB[_],FC[_],FD[_]](
    e1: Extraction[FA,A], e2: Extraction[FB,B], e3: Extraction[FC,C], e4: Extraction[FD,D]
  ) extends Extraction[Id,(FA[A], FB[B], FC[C], FD[D])] {
    override type I = this.type

    override def extract(stringProducer: RawValueProducer): Either[ExtractionErrors, (FA[A], FB[B], FC[C], FD[D])] = {
      val r1 = e1.extract(stringProducer)
      val r2 = e2.extract(stringProducer)
      val r3 = e3.extract(stringProducer)
      val r4 = e4.extract(stringProducer)
      Apply[Either[ExtractionErrors, ?]].map4(r1, r2, r3, r4)((a, b, c, d) => (a, b, c, d))
    }
  }

}

