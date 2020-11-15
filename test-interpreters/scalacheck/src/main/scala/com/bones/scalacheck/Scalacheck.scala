package com.bones.scalacheck

import cats.Applicative
import com.bones.data.template.{KvpCollectionEncoder, KvpCollectionTransformation}
import com.bones.data.values.CNilF
import com.bones.data.{KvpCoNil, KvpCoproduct, KvpCoproductCollectionHead, _}
import org.scalacheck._
import shapeless.{:+:, CNil, Coproduct, HList, HNil, Inl, Inr, Nat}

object GenValue {

  /** using kind projector allows us to create a new interpreter by merging two existing interpreters.
    * see https://stackoverflow.com/a/60561575/387094
    * */
  def merge[L[_], R[_] <: Coproduct, A](
    li: GenValue[L],
    ri: GenValue[R]): GenValue[Lambda[A => L[A] :+: R[A]]] =
    new GenValue[Lambda[A => L[A] :+: R[A]]] {

      override def gen[A](lr: L[A] :+: R[A]): Gen[A] = lr match {
        case Inl(l) => li.gen(l)
        case Inr(r) => ri.gen(r)
      }
    }

  implicit class InterpreterOps[ALG[_], OUT](val base: GenValue[ALG]) extends AnyVal {
    def ++[R[_] <: Coproduct](r: GenValue[R]): GenValue[Lambda[A => ALG[A] :+: R[A]]] =
      merge(base, r)
  }

  object CNilGenEncoder extends GenValue[CNilF] {
    override def gen[A](ag: CNilF[A]): Gen[A] = sys.error("Unreachable code")
  }

}

/**
  * Implement this to support a custom Algebra.
  *
  * @tparam ALG
  */
trait GenValue[ALG[_]] {
  def gen[A](ag: ALG[A]): Gen[A]
}

trait ScalacheckBase[K, ALG[_]] extends KvpCollectionTransformation[K, ALG, Gen] {

  val genValue: GenValue[ALG]

  def generateGen[A](collection: KvpCollection[K, ALG, A]): Gen[A] =
    fromKvpCollection(collection)

  override implicit def applicativeOfOut: Applicative[Gen] = new Applicative[Gen] {
    override def pure[A](x: A): Gen[A] = Gen.const(x)

    override def ap[A, B](ff: Gen[A => B])(fa: Gen[A]): Gen[B] = {
      fa.flatMap(a => {
        ff.map(f => f(a))
      })
    }
  }

  override def primitiveEncoder[A](keyDefinition: KeyDefinition[K, ALG, A]): Gen[A] =
    determineValueDefinition(keyDefinition.dataDefinition)

  private def coproductFrequencies[O <: Coproduct](
    kvpCoproduct: KvpCoproduct[K, ALG, O]): List[(Int, Gen[O])] = {
    kvpCoproduct match {
      case kvpCoproductCollectionHead: KvpCoproductCollectionHead[K, ALG, h, c, O] => {
        val tailFrequencies =
          coproductFrequencies[c](kvpCoproductCollectionHead.kvpTail)
            .map(freq => (freq._1, freq._2.map(o => Inr(o).asInstanceOf[O])))
        val headGen = fromKvpCollection(kvpCoproductCollectionHead.kvpCollection)
          .map(h => Inl(h).asInstanceOf[O])
        (1, headGen) :: tailFrequencies
      }
      case _: KvpCoNil[K, ALG] => List.empty[(Int, Gen[O])]
    }
  }
  override def kvpCoproductCollectionHead[A, C <: Coproduct, O <: A :+: C](
    kvpCoproductCollectionHead: KvpCoproductCollectionHead[K, ALG, A, C, O]): Gen[O] = {

    val frequencies = coproductFrequencies(kvpCoproductCollectionHead)
    Gen.frequency(frequencies: _*)

  }

  def determineValueDefinition[A](value: Either[HigherOrderValue[K, ALG, A], ALG[A]]): Gen[A] = {
    value match {
      case Left(kvp)  => valueDefinition(kvp)
      case Right(alg) => genValue.gen(alg)
    }
  }

  def valueDefinition[A](fgo: HigherOrderValue[K, ALG, A]): Gen[A] =
    fgo match {
      case op: OptionalValue[K, ALG, a] @unchecked => {
        val optionalGen = determineValueDefinition(op.valueDefinitionOp).map(Some(_))
        Gen.frequency(
          (9, optionalGen),
          (1, None)
        )
      }
      case ld: ListData[K, ALG, t] @unchecked =>
        implicit val elemGenerator = determineValueDefinition(ld.tDefinition)
        for {
          numElems <- Gen.choose(1, 500)
          elem <- Gen.listOfN(numElems, elemGenerator)
        } yield elem
      case ed: EitherData[K, ALG, a, b] @unchecked => {
        val left = determineValueDefinition(ed.definitionA).map(Left(_))
        val right = determineValueDefinition(ed.definitionB).map(Right(_))
        Gen.frequency((1, left), (1, right))
      }
      case kvp: KvpCollectionValue[K, ALG, A] @unchecked =>
        fromKvpCollection(kvp.kvpCollection)
    }

}
