package com.bones.fullstack

import cats.effect.IO
import com.bones.Definitions.{Error, Person, WithId, personSchema}
import com.bones.data.Value.BonesSchema
import com.bones.jdbc.{DbDelete, DbGet, DbInsertValues, DbUpdateValues}
import fs2.Stream
import javax.sql.DataSource

case class CrudDbDefinitions[A](schema: BonesSchema[A], ds: DataSource) {

  val searchF: Stream[IO, WithId[Person]] = Stream.empty


  private val insertQuery = DbInsertValues.insertQuery(personSchema)(ds)
  val createF: Person => IO[Either[Error,WithId[Person]]] = {
    (person:Person) => IO {
      insertQuery(person)
        .right.map(pid => WithId(pid._1, pid._2))
        .left.map(ex => Error(ex.toString))
    }
  }

  private val readQuery = DbGet.getEntity(personSchema)(ds)
  val readF: Long => IO[Either[Error, WithId[Person]]] =
    id => IO {
      readQuery(id)
        .right.map(pid => WithId(id, pid))
        .left.map(ex => Error(ex.toString()))
    }


  val updateQuery = DbUpdateValues.updateQuery(personSchema)(ds)
  val updateF: (Long, Person) => IO[Either[Error, WithId[Person]]] =
    (id, person) => IO {
      updateQuery(id,person)
        .right.map(pid => WithId(id, pid))
        .left.map(ex => Error(ex.toString()))
    }

  val deleteQuery = DbDelete.delete(personSchema)(ds)
  val deleteF: Long => IO[Either[Error, WithId[Person]]] =
    (id) => IO {
      deleteQuery(id)
        .right.map(pid => WithId(id, pid))
        .left.map(ex => Error(ex.toString()))
    }
}
