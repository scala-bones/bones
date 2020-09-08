package com.bones.jdbc

import java.util.Properties

import com.bones.schemas.Schemas
import com.bones.syntax._
import org.scalatest.funsuite.AnyFunSuite

class DbGetInterpreterTest extends AnyFunSuite {

  import java.sql.{Connection, DriverManager}

  ignore("load entity ") {
    val url = "jdbc:postgresql://localhost/tstevens"
    val props = new Properties()
    val conn: Connection = DriverManager.getConnection(url)

    val idDefinition = IdDefinition("id", long())

    val result = com.bones.jdbc.dbGetDefaultInterpreter
      .getEntity(Schemas.creditCardSchema, idDefinition)(2)(conn)

    //    println(result)

  }

}
