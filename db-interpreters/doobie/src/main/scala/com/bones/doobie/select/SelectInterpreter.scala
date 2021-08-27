package com.bones.doobie.select

import com.bones.data.Error.ExtractionErrors
import com.bones.data.KvpCollection
import doobie.ConnectionIO
import doobie.free.connection.raw

trait SelectInterpreter[ALG[_]] {

  val jdbcSelectInterpreter: com.bones.jdbc.select.SelectInterpreter[ALG]

  def select[A, ID](
    kvp: KvpCollection[String, ALG, A],
    idSchema: KvpCollection[String, ALG, ID],
    tableNameOverride: Option[String] = None
  ): ID => ConnectionIO[Either[ExtractionErrors[String], (ID, A)]] = {
    val selectF = jdbcSelectInterpreter.selectEntity(kvp, idSchema, tableNameOverride)
    id => {
      raw(con => {
        selectF(id)(con)
      })
    }
  }
}
