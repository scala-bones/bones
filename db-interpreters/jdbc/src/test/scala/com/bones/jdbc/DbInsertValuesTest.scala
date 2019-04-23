package com.bones.jdbc

import java.util.{Properties, UUID}

import com.bones.jdbc.{DbInsertValues, DbUpdateValues}
import com.bones.schemas.{JavaCurrencyEnum, Schemas}
import com.bones.schemas.Schemas.{BillingLocation, CC, CreditCardTypes, Currency}
import org.scalatest.FunSuite

class DbInsertValuesTest extends FunSuite {


  ignore("Insert into db test") {

    val cc = CC("12345", "7890", UUID.randomUUID(), UUID.randomUUID(), CreditCardTypes.Mastercard, 8, 2020, "Kurt Vonnegut", JavaCurrencyEnum.CAD, Currency.CAD, None, UUID.randomUUID(), Some(BillingLocation("US", None)))

    val result = DbInsertValues.insertQueryWithConnection(Schemas.creditCardSchema)(cc)

    import java.sql.Connection
    import java.sql.DriverManager

    val url = "jdbc:postgresql://localhost/tstevens"
    val props = new Properties()
    val conn: Connection = DriverManager.getConnection(url)


    val executionResult = result(conn)

    val newCC = cc.copy(expMonth = 12, expYear = 2012, billingLocation = None)
    DbUpdateValues.updateQueryWithConnection(Schemas.creditCardSchema)(1, newCC)(conn)


//    println(executionResult)
  }




}
