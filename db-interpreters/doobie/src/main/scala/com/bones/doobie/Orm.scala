package com.bones.doobie

import cats.effect._
import cats.implicits._
import doobie._
import doobie.implicits._
import fs2.Stream
import shapeless._
import shapeless.ops.hlist._
import shapeless.ops.record._

object Orm {

  // to silence unused warnings
  def void[A](a: A): Unit = (a, ())._2


  trait Dao[A] {
    type Key
    def insert(a: A): ConnectionIO[Key]
    def find(k: Key): ConnectionIO[Option[A]]
    def findAll: Stream[ConnectionIO, (Key, A)]
    def update(k: Key, a: A): ConnectionIO[Int]
    def delete(k: Key): ConnectionIO[Int]
  }
  object Dao {

    type Aux[A, K] = Dao[A] { type Key = K }

    def apply[A](implicit ev: Dao[A]): Aux[A, ev.Key] = ev

    object derive {
      def apply[A, K] = new Partial[A, K]
      class Partial[A, K] {
        def apply[R <: HList, S <: HList](table: String, keyCol: String)(
          implicit ev: LabelledGeneric.Aux[A, R],
          ra: Read[A],
          wa: Write[A],
          ks: Keys.Aux[R, S],
          tl: ToList[S, Symbol],
          rk: Read[K],
          wk: Write[K]
        ): Aux[A, K] =
          new Dao[A] {
            void(ev)

            type Key = K
            val cols = ks.apply.toList.map(_.name)

            def insert(a: A): ConnectionIO[Key] =
              Update[A](s"""
                INSERT INTO $table (${cols.mkString(", ")})
                VALUES (${cols.as("?").mkString(", ")})
              """).withUniqueGeneratedKeys[Key](keyCol)(a)

            def find(key: Key): ConnectionIO[Option[A]] =
              Query[Key, A](s"""
                SELECT ${cols.mkString(", ")}
                FROM $table
                WHERE $keyCol = ?
              """).option(key)

            def findAll: Stream[ConnectionIO, (Key, A)] =
              Query0[(Key,A)](s"""
                SELECT $keyCol, ${cols.mkString(", ")}
                FROM $table limit 100
              """).stream

            def update(k: Key, a: A): ConnectionIO[Int] =
              Update[(A, Key)](s"""
                UPDATE $table
                SET ${cols.map(_ + " = ?").mkString(", ")}
                WHERE $keyCol = ?
              """).run((a, k))

            def delete(k: Key): ConnectionIO[Int] = {
              Update[Key](s"""
                DELETE FROM $table
                WHERE $keyCol = ?
              """).run(k)
            }

          }
      }
    }

  }
}
