package com.bones.scalacheck

import com.bones.data.custom.CNilF
import com.bones.data.{KvpCoNil, KvpCoproduct, KvpSingleValueLeft, _}
import org.scalacheck._
import shapeless.{:+:, Coproduct, HList, HNil, Inl, Inr, Nat}

object GenAlg {

  /** using kind projector allows us to create a new interpreter by merging two existing interpreters.
    * see https://stackoverflow.com/a/60561575/387094
    * */
  def merge[L[_], R[_] <: Coproduct, A](
    li: GenAlg[L],
    ri: GenAlg[R]): GenAlg[Lambda[A => L[A] :+: R[A]]] =
    new GenAlg[Lambda[A => L[A] :+: R[A]]] {

      override def gen[A](lr: L[A] :+: R[A]): Gen[A] = lr match {
        case Inl(l) => li.gen(l)
        case Inr(r) => ri.gen(r)
      }
    }

  implicit class InterpreterOps[ALG[_], OUT](val base: GenAlg[ALG]) extends AnyVal {
    def ++[R[_] <: Coproduct](r: GenAlg[R]): GenAlg[Lambda[A => ALG[A] :+: R[A]]] =
      merge(base, r)
  }

  object CNilGenEncoder extends GenAlg[CNilF] {
    override def gen[A](ag: CNilF[A]): Gen[A] = sys.error("Unreachable code")
  }

}

/**
  * Implement this to support a custom Algebra.
  *
  * @tparam ALG
  */
trait GenAlg[ALG[_]] {
  def gen[A](ag: ALG[A]): Gen[A]
}

object Scalacheck extends ScalacheckBase

trait ScalacheckBase {

  def createCustomGen[ALG[_], A](bonesSchema: KvpCollection[ALG, A], genAlg: GenAlg[ALG]): Gen[A] =
    bonesSchema match {
      case co: KvpCoproductConvert[ALG, c, a] => valueDefinition(co, genAlg)
      case co: HListConvert[ALG, h, n, a]     => valueDefinition(co, genAlg)
    }

  def kvpCoproduct[ALG[_], C <: Coproduct](
    co: KvpCoproduct[ALG, C],
    genAlg: GenAlg[ALG]): List[Gen[C]] = {
    co match {
      case nil: KvpCoNil[_] => List.empty
      case co: KvpSingleValueLeft[ALG, a, r] @unchecked =>
        val head = determineValueDefinition[ALG, a](co.kvpValue, genAlg).map(Inl(_))
        val tail = kvpCoproduct(co.kvpTail, genAlg).map(gen => gen.map(Inr(_)))
        head :: tail
    }
  }

  def kvpHList[ALG[_], H <: HList, HL <: Nat](
    group: KvpHList[ALG, H, HL],
    genAlg: GenAlg[ALG]): Gen[H] = {
    group match {
      case ni: KvpNil[_] => Gen.const(HNil)
      case op: KvpSingleValueHead[ALG, h, t, tl, a] @unchecked =>
        implicit val isHCons = op.isHCons
        val headGen = determineValueDefinition(op.fieldDefinition.dataDefinition, genAlg)
        val tailGen = kvpHList(op.tail, genAlg)
        val result: Gen[H] = for {
          head <- headGen
          tail <- tailGen
        } yield {
          op.isHCons.cons(head, tail)
        }
        result
      case op: KvpHListHead[ALG, a, al, h, hl, t, tl] @unchecked =>
        implicit val prepend = op.prepend
        val headGen = kvpHList(op.head, genAlg)
        val tailGen = kvpHList(op.tail, genAlg)
        for {
          head <- headGen
          tail <- tailGen
        } yield {
          head ::: tail
        }
      case op: KvpConcreteTypeHead[ALG, a, ht, nt] @unchecked =>
        val headGen = fromCustomSchema(op.bonesSchema, genAlg)
        val tailGen = kvpHList(op.tail, genAlg)
        for {
          a <- headGen
          tail <- tailGen
        } yield {
          op.isHCons.cons(a, tail)
        }
    }
  }

  def fromCustomSchema[ALG[_], A](bonesSchema: KvpCollection[ALG, A], genAlg: GenAlg[ALG]): Gen[A] = {
    bonesSchema match {
      case hl: HListConvert[ALG, h, n, a] => kvpHList(hl.from, genAlg).map(hl.fHtoA)
      case co: KvpCoproductConvert[ALG, c, a] => {
        val gens = kvpCoproduct(co.from, genAlg).map(genList => genList.map(co.cToA)).map((1, _))
        Gen.frequency(gens: _*)
      }
    }
  }

  def determineValueDefinition[ALG[_], A](
                                           value: Either[KvpCollection[ALG, A], ALG[A]],
                                           genAlg: GenAlg[ALG]): Gen[A] = {
    value match {
      case Left(kvp)  => valueDefinition(kvp, genAlg)
      case Right(alg) => genAlg.gen(alg)
    }
  }

  def valueDefinition[ALG[_], A](fgo: KvpCollection[ALG, A], genAlg: GenAlg[ALG]): Gen[A] =
    fgo match {
      case op: OptionalKvpValueDefinition[ALG, a] @unchecked => {
        val optionalGen = determineValueDefinition(op.valueDefinitionOp, genAlg).map(Some(_))
        Gen.frequency(
          (9, optionalGen),
          (1, None)
        )
      }
      case ld: ListData[ALG, t] @unchecked =>
        implicit val elemGenerator = determineValueDefinition(ld.tDefinition, genAlg)
        for {
          numElems <- Gen.choose(1, 500)
          elem <- Gen.listOfN(numElems, elemGenerator)
        } yield elem
      case ed: EitherData[ALG, a, b] @unchecked => {
        val left = determineValueDefinition(ed.definitionA, genAlg).map(Left(_))
        val right = determineValueDefinition(ed.definitionB, genAlg).map(Right(_))
        Gen.frequency((1, left), (1, right))
      }
      case kvp: KvpHListValue[ALG, h, hl] @unchecked =>
        kvpHList(kvp.kvpHList, genAlg).map(_.asInstanceOf[A])
      case co: KvpCoproductValue[ALG, c] @unchecked =>
        // Get a list of coproduct and gen them with equal frequency (1)
        val gens = kvpCoproduct(co.kvpCoproduct, genAlg).map(_.map(_.asInstanceOf[A])).map((1, _))
        Gen.frequency(gens: _*)
      case x: HListConvert[ALG, a, al, b] @unchecked =>
        kvpHList(x.from, genAlg).map(hList => x.fHtoA(hList))
      case co: KvpCoproductConvert[ALG, c, a] @unchecked =>
        val gens = kvpCoproduct(co.from, genAlg).map((1, _))
        Gen.frequency(gens: _*).map(coproduct => co.cToA(coproduct))
    }




}
