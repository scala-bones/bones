package com.bones.doobie.select

import java.sql.Connection

import cats.data.NonEmptyList
import com.bones.data.Error.ExtractionError
import com.bones.data.KvpCollection
import com.bones.jdbc.IdDefinition
import doobie.ConnectionIO
import doobie.free.connection.raw

trait SelectInterpreter[ALG[_]] {

  val jdbcSelectInterpreter: com.bones.jdbc.select.SelectInterpreter[ALG]

  def select[A, ID](kvp: KvpCollection[ALG, A], idSchema: KvpCollection[ALG, ID])
    : ID => ConnectionIO[Either[NonEmptyList[ExtractionError], (ID, A)]] = {
    val selectF = jdbcSelectInterpreter.selectEntity(kvp, idSchema)
    id =>
      {
        raw(con => {
          selectF(id)(con)
        })
      }
  }
}
