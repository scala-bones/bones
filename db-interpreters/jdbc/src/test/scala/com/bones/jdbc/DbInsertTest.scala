package com.bones.jdbc

import java.util.{Properties, UUID}

import com.bones.jdbc.update.defaultDbUpdate
import com.bones.schemas.Schemas
import com.bones.schemas.Schemas._
import com.bones.syntax._
import org.scalatest.funsuite.AnyFunSuite

class DbInsertTest extends AnyFunSuite {

  ignore("Insert into db test") {

    val cc = CC(
      "12345",
      "7890",
      UUID.randomUUID(),
      UUID.randomUUID(),
      CreditCardType.Mastercard,
      8,
      2020,
      "Kurt Vonnegut",
      Currency.CAD,
      None,
      UUID.randomUUID(),
      Some(BillingLocation("US", None))
    )

    val result =
      com.bones.jdbc.insert.defaultDbInsertInterpreter
        .insertQueryWithConnection(Schemas.creditCardSchema, Schemas.idSchema)(cc)

    import java.sql.{Connection, DriverManager}

    val url = "jdbc:postgresql://localhost/tstevens"
    val props = new Properties()
    val conn: Connection = DriverManager.getConnection(url)

    val executionResult = result(conn)

    val newCC = cc.copy(expMonth = 12, expYear = 2012, billingLocation = None)
    val idDef = (("id", long(lv.positive)) :: kvpNil).encodedHead[Long]()
    defaultDbUpdate.updateQuery(Schemas.creditCardSchema, idDef)(1, newCC)(conn)
//    println(executionResult)
  }

}
