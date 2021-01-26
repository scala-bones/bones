package com.bones.slick

import com.bones.data.values.DefaultValues
import com.bones.data.{KvpHListCollection, KvpNil, KvpWrappedHList}
import org.scalatest.funspec.AnyFunSpec
import shapeless._
import slick.lifted.{AbstractTable, Rep, Tag}
import com.bones.syntax._
import shapeless.ops.hlist._
import slick.jdbc.PostgresProfile.api._
import slickless._

class SlickTest { // extends AnyFunSpec {

  // Definition of the SUPPLIERS table
  class Suppliers(tag: Tag)
      extends Table[(Int, String, String, String, String, String)](tag, "SUPPLIERS") {
    def id = column[Int]("SUP_ID", O.PrimaryKey) // This is the primary key column
    def name = column[String]("SUP_NAME")
    def street = column[String]("STREET")
    def city = column[String]("CITY")
    def state = column[String]("STATE")
    def zip = column[String]("ZIP")
    // Every table needs a * projection with the same type as the table's type parameter
    def * = (id, name, street, city, state, zip)
  }
  val suppliers = TableQuery[Suppliers]

  // Definition of the COFFEES table
  class Coffees(tag: Tag)
      extends Table[String :: Int :: Double :: Int :: Int :: HNil](tag, "COFFEES") {
    def name = column[String]("COF_NAME", O.PrimaryKey)
    def supID = column[Int]("SUP_ID")
    def price = column[Double]("PRICE")
    def sales = column[Int]("SALES")
    def total = column[Int]("TOTAL")
    def * = name :: supID :: price :: sales :: total :: HNil
    // A reified foreign key relation that can be navigated to create a join
    def supplier = foreignKey("SUP_FK", supID, suppliers)(_.id)
  }
  val coffees = TableQuery[Coffees]

  coffees.map(c => c.name)

  case class KvpSlick[C](jsonName: String, slickColumn: Rep[C])

  val coffeeSchema =
    ("name", string(sv.unique)) ::
      ("subID", int(iv.positive)) ::
      ("price", double(dv.positive)) ::
      ("sales", int(iv.positive)) ::
      ("total", int(iv.positive)) ::
      KvpNil[String, DefaultValues]

  val coffeeSchemaTupled = coffeeSchema.tupled

//  trait InsertH[H <: HList] {}
//  trait MapRep[H <: HList, HR <: HList, E, C <: TableQuery[E]] {
//    val ev: Mapped.Aux[H, Rep, HR]
//    def apply(tq: C => HR): InsertH[H]
//  }
//
//  def coffeeMap[ALG[_], H <: HList, HL <: Nat, E, C <: TableQuery[E], HR <: HList](
//    kvpHListCollection: KvpHListCollection[String, ALG, H, HL],
//    tq: C)(implicit impEv: Mapped.Aux[H, Rep, HR]) = new MapRep[H, HR, E, C] {
//    override val ev: Mapped.Aux[H, Rep, HR] = impEv
//    val tableQuery = tq
//    override def apply(f: C => HR): InsertH[H] = new InsertH[H] {
//      val hReps = f.apply(tq)
//      hReps
//    }
//  }
//
//  coffeeMap(coffeeSchema, coffees)
//
//  type CoffeeF[A] = Coffees => Rep[A]

//  trait Insert[A]
//  object Insert {
//    // Here is a little trick to proof that for any T that
//    // happened to be a subclass of Field[A] the Out is A
//    implicit def fieldInserter[T, A](implicit ev: T <:< Insert[A]): Inserter.Aux[T, A] =
//      new Inserter[T] {
//        override type Out = A
//      }
//  }
//
//  // The extractor for A
//  trait Inserter[A] {
//    type Out // Produces result of type Out
//  }
//  object Inserter {
//    type Aux[A, Out0] = Inserter[A] {
//      type Out = Out0
//    }
//    // Proof that Out for HNil is HNil
//    implicit val hnilExtractor: Aux[HNil, HNil] =
//      new Inserter[HNil] {
//        override type Out = HNil
//      }
//    // Proof that Out for T :: H is hlist of extractor result for H and T
//    implicit def hconsExtractor[H, HO, T <: HList, TO <: HList](
//      implicit H: Aux[H, HO],
//      T: Aux[T, TO]): Aux[H :: T, HO :: TO] =
//      new Inserter[H :: T] {
//        override type Out = HO :: TO
//      }
//
//  }
//
//  val mapped: (Coffees => Rep[String]) ::
//    (CoffeeF[Int]) ::
//    (CoffeeF[Double]) ::
//    (CoffeeF[Int]) ::
//    (CoffeeF[Int]) ::
//    HNil = { (c: Coffees) =>
//    c.name
//  } :: { (c: Coffees) =>
//    c.supID
//  } :: { (c: Coffees) =>
//    c.price
//  } :: { (c: Coffees) =>
//    c.sales
//  } :: { (c: Coffees) =>
//    c.total
//  } ::
//    HNil
//
//  val c2 = ("name", string(sv.unique)) ::
//    KvpNil[String, DefaultValues]
//  val mapped2: Rep[String] :: HNil = coffees.baseTableRow.name :: HNil
//
//  type ApplyE[E, A] = E => Rep[A]
//
//  def createInsert[
//    H <: HList,
//    HL <: Nat,
//    HH <: HList,
//    ID,
//    MCOUT <: HList,
//    E <: AbstractTable[_],
//    C <: TableQuery[E]](
//    h: H,
//    reps: HH, //Coffees to Rep[ ]
//    query: C)(
//    implicit
//    ev: Mapped.Aux[H, ApplyE[E, *], HH], // Proof that CoffeeF[Int] :: HNil same as Mapped, Int :: HNil
//    mc: MapCons.Aux[E, HH, H], // Pro0f that CoffeeF[Int].apply(cofffee) == Int
//    mct: Tupler[H],
//    cm: ConstMapper.Aux[HH, E, H]): ID = {
//
//    query
//      .map(c => {
//        reps.mapCons(c)
//      }) += h
//    ???
//  }

}
