package com.bones.jdbc

import java.util.{Properties, UUID}

import com.bones.jdbc.insert.DbInsert
import com.bones.jdbc.update.{DbUpdate, defaultDbUpdate}
import com.bones.syntax._
import com.bones.schemas.Schemas
import com.bones.schemas.Schemas._
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
        .insertQueryWithConnectionCustomAlgebra(Schemas.creditCardSchema, Schemas.idSchema)(cc)

    import java.sql.{Connection, DriverManager}

    val url = "jdbc:postgresql://localhost/tstevens"
    val props = new Properties()
    val conn: Connection = DriverManager.getConnection(url)

    val executionResult = result(conn)

    val newCC = cc.copy(expMonth = 12, expYear = 2012, billingLocation = None)
    val idDef = IdDefinition("id", long(lv.positive))
    defaultDbUpdate.updateQuery(Schemas.creditCardSchema, idDef)(1, newCC)(conn)
//    println(executionResult)
  }

}
