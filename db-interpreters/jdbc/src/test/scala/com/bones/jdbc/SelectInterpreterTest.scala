package com.bones.jdbc

import java.util.Properties

import com.bones.schemas.Schemas
import com.bones.syntax._
import org.scalatest.funsuite.AnyFunSuite

class SelectInterpreterTest extends AnyFunSuite {

  import java.sql.{Connection, DriverManager}

  ignore("load entity ") {
    val url = "jdbc:postgresql://localhost/tstevens"
    val props = new Properties()
    val conn: Connection = DriverManager.getConnection(url)

    val id = (("id", long()) :: kvpNil).encodedHead[Long]()

    val result = com.bones.jdbc.select.defaultSelectInterpreter
      .selectEntity(Schemas.creditCardSchema, id)(2)(conn)

    //    println(result)

  }

}
