package com.bones.jdbc

import java.util.Properties

import com.bones.schemas.Schemas
import org.scalatest.funsuite.AnyFunSuite

class DbGetTest extends AnyFunSuite {

  import java.sql.Connection
  import java.sql.DriverManager

  ignore("load entity ") {
    val url = "jdbc:postgresql://localhost/tstevens"
    val props = new Properties()
    val conn: Connection = DriverManager.getConnection(url)

    val result = DbGet.getEntityWithConnection(Schemas.creditCardSchema)(2)(conn)

    //    println(result)

  }

}
