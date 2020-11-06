package com.bones.doobie.insert

import com.bones.data.Error.ExtractionErrors
import com.bones.data.KvpCollection
import com.bones.jdbc.insert.DbInsert
import doobie.ConnectionIO
import doobie.free.connection.raw

/**
  * The current implementation wraps the jdbc query in a Raw operation.
  * @tparam ALG
  */
trait InsertInterpreter[ALG[_]] {

  val insert: DbInsert[ALG]

  def insertAll[A, ID](collection: KvpCollection[String, ALG, A])
    : Seq[A] => ConnectionIO[Either[ExtractionErrors[String], Int]] = {
    val insertF = insert.batchInsertQueryWithConnection(collection)
    aList =>
      raw(con => insertF(aList)(con))
  }

  def insert[A, ID](
    collection: KvpCollection[String, ALG, A],
    idSchema: KvpCollection[String, ALG, ID])
    : A => ConnectionIO[Either[ExtractionErrors[String], ID]] = {
    val insertF = insert.insertQueryWithConnection(collection, idSchema)
    a =>
      {
        raw(con => {
          insertF(a)(con)
        })
      }

  }

}
