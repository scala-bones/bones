package com.bones.jdbc.integration

import com.bones.jdbc.ideal.TableCollection
import com.bones.schemas.Schemas
import com.bones.si.ideal.{Diff, IdealSchema}
import com.bones.si.jdbc.load.{DatabaseMetadataCache, DatabaseQuery}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers

class JdbcIntegrationTest extends AnyFunSuite with Matchers {

  test("full integration with create table, insert, select, update and get") {

    val allSupported = Schemas.allSupportCaseClass

    val tableCollection = com.bones.jdbc.ideal.defaultIdealInterpreter.toIdeal(allSupported)

    val pkgpiSchemaQuery = DatabaseQuery.everything.schemas("public")
    val pkgpiIdealSchema = IdealSchema("public", tableCollection.allTables)

    val dbCache = DatabaseMetadataCache.empty

    val diff = Diff.findDiff(dbCache, pkgpiIdealSchema)

    true

  }
}
