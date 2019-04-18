package com.bones.doobie

import java.util.{Properties, UUID}

import com.bones.schemas.{JavaCurrencyEnum, Schemas}
import com.bones.schemas.Schemas.{BillingLocation, CC, CreditCardTypes, Currency}
import org.scalatest.FunSuite

class DbInsertValuesTest extends FunSuite {

  val cc = CC("12345", "7890", UUID.randomUUID(), UUID.randomUUID(), CreditCardTypes.Mastercard, 8, 2020, "Kurt Vonnegut", JavaCurrencyEnum.CAD, Currency.CAD, None, UUID.randomUUID(), Some(BillingLocation("US", None)))

  val result = DbInsertValues.insertQuery(Schemas.creditCardSchema)(cc)

  import java.sql.Connection
  import java.sql.DriverManager

  val url = "jdbc:postgresql://localhost/tstevens"
  val props = new Properties()
  val conn: Connection = DriverManager.getConnection(url)


  val executionResult = result(conn)





  println(executionResult)




}
