package com.bones.jdbc

import java.util.Properties

import com.bones.schemas.Schemas
import com.bones.syntax._
import org.scalatest.funsuite.AnyFunSuite

class DbGetTest extends AnyFunSuite {

  import java.sql.{Connection, DriverManager}

  ignore("load entity ") {
    val url = "jdbc:postgresql://localhost/tstevens"
    val props = new Properties()
    val conn: Connection = DriverManager.getConnection(url)

    val idDefinition = IdDefinition("id", long())

    val result = DbGet.getEntityWithConnectionCustomAlgebra(Schemas.creditCardSchema, idDefinition, com.bones.jdbc.rs.defaultResultSetInterpreter, update.defaultDbUpdateInterpreter)(2)(conn)

    //    println(result)

  }

}
