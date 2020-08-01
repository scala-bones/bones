package com.bones.liquibase

import com.bones.liquibase.value.DefaultScalaCoreDatabaseObject
import com.bones.schemas.ScalaCoreSchema
import liquibase.CatalogAndSchema
import liquibase.database.{Database, DatabaseFactory}
import liquibase.resource.ClassLoaderResourceAccessor
import liquibase.snapshot.{SnapshotControl, SnapshotGeneratorFactory}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers

class GenerateDatabaseObjectTest extends AnyFunSuite with Matchers {

  ignore("should work for scala core") {

    val objects = GenerateDatabaseObject.generateDatabaseObjects(
      ScalaCoreSchema.schema,
      ScalaCoreSchema.idSchema,
      DefaultScalaCoreDatabaseObject)

    objects

  }

  test("load existing db") {

    val url = "jdbc:postgresql://localhost/postgres"
    val username = "travis"
    val password = ""
    val resourceAccessor = new ClassLoaderResourceAccessor(Thread.currentThread().getContextClassLoader)
    val driver = "org.postgresql.Driver"
    val databaseClass = null
    val driverPropertiesFile = null
    val propertyProviderClass = null

    val database = DatabaseFactory.getInstance.openDatabase(
      url,
      username,
      password,
      driver,
      databaseClass,
      driverPropertiesFile,
      propertyProviderClass,
      resourceAccessor)

    val schemas = Array[CatalogAndSchema](database.getDefaultSchema)
    val snapshotControl = new SnapshotControl(database)

    val snapshot = SnapshotGeneratorFactory.getInstance.createSnapshot(schemas, database, snapshotControl)

    snapshot


  }
}
