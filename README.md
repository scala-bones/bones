# Bones

** Please note that the Bones project is currently a work in progress. **

## Purpose

The purpose of this library is to remove as much boilerplate as possible out of a CRUD/REST application.
There are 3 steps to create a new CRUD application.
 * Define the input and output and error schema
 * Pass the schemas to an interpreter
 * Provide methods for to create/read/update/delete data from a data store.
 
 
## Data Types.

Bones currently supports the following data types natively.

```
String, Int, Boolean, Either, Long, List, Short, Float, Double, BigDecimal, Byte Array, LocalDateTime,
LocalDate, UUID, Enumeration
```

Product Types (Shapeless HList/ Case classes) and Sum Types (traits/case classes or Shapeless Coproducts) are also supported.

Note: *Custom user defined types are currently not supported.*  For instance, something like 
the java.io.File type can not be represented.  TODO: The goal of the next version (0.6.0) is to 
make the interpreters coproduct aware, in order to support custom user defined types.  
 

### Schema DSL
Bones defines a Domain Specific Language (DSL) for describing CRUD applications with validation.

Using the DSL will result in creating a Generalized Algebraic Data Type (GADT) data structure.
The GADT structure describes the data, validation and available Create/Read/Update/Delete (CRUD) actions.
 
As an example, we will describe a Person CRUD application.  For a complete running service writing to the Database,
refer to the [Example](https://github.com/OleTraveler/bones/blob/master/examples/http4s-examples/src/main/scala/com/bones/PersonEndpoint.scala
)

```$scala

import com.bones.syntax._

case class Person(name: String, age: Int)

val personSchema = (
  kvp("name", string(sv.matchesRegex("^[a-zA-Z ]*$".r))) ::
  kvp("age", int(iv.min(0))) ::
  KvpNil
).convert[Person]

case class Error(error: String)
  
```


### Interpreters

The power in creating a GADT structure, is that we can provide different interpreters for the defined Schema.
Once the basic functionality of the interpreter is created, we can reuse the interpreter passing in different schemas
to create common functionality.

I other words, if services perform similar behavior, 
such as write to Kafka, S3, a Database or an external services, the boilerplate code can be reduced by using GADTs.
This project is to provide a common vocabulary for any service, however this project is currently focused on 
REST CRUD apps.
 
This example uses the Scala http4s with schema defined above and for less than 11 LINES OF CODE, provides full CRUD functionality writing to 
a JDBC datasource.  It provides 3 interchange formats: JSON, BSON and Protobuf (yes, protobuf!), 
and finally a protofile describing the data and OAS3 compliant Schema.  The idea is that this will work for any Bones Schema Values
(although there are currently some limitation on Coproduct which will be fixed soon).


```$scala
/** Example endpoint.  This creates a complete application which saves a person to a local database including:
  * JSON endpoints, Protobuf Endpoints, 5 CRUD Endpoints (Get, Put, Post, Delete, Search All),
  * Swagger, DB DDL.
  */

object PersonEndpoint extends LocalhostAllIOApp {

  val ds: HikariDataSource = localhostDataSource

  case class Person(name: String, age: Long, gender: Option[String])

  val personSchema = (
    kvp("name", string(sv.matchesRegex("^[a-zA-Z ]*$".r))) ::
      kvp("age", long(iv.min(0))) ::
      kvp("gender", string.optional) ::
      KvpNil
    ).convert[Person]


  override def services: HttpRoutes[IO] =
    serviceRoutesWithCrudMiddleware("person", personSchema, ds)
}
```

### List of Interpreters

Serializer / Marhsaller Interpreters
* [Circe](interchange-format-interpreters/circe/README.md)
* [BSON](interchange-format-interpreters/bson/README.md)
* [Argonaut](interchange-format-interpreters/argonaut/README.md)
* [lift-json](interchange-format-interpreters/lift-json/README.md)
* [protobuf](interchange-format-interpreters/protobuf/README.md)
* [scalatest](test-interpreters/scalacheck/README.md)

HTTP REST Interpreters
* [http4s](rest-interpreters/http4s-interpreter/README.md)

Incomplete Interpreters
* [swagger](interchange-format-interpreters/swagger-oas3/README.md) - Does not currently support Coproduc
* [dbJdbc](db-interpreters/jdbc/README.md) - Does not currently support coproducts
* [React](client-interpreters/react/README.md) - This is more of a POC.  Does not currently support Coproducts
* [http4s-client](client-interpreters/http4s-client/README.md) - Not Implemented 


## Download

Version 0.5.0 includes validation, CRUD and the http4s REST Interpreter.


#### Http4s Circe Interpreter (currently the only interpreter)
Basic CRUD capabilities implemented with http4s, circe and doobie.
```libraryDependencies += "com.github.oletraveler" %% "bones-http4s-circe" % "0.5.0"```

### CI
[![Build Status](https://travis-ci.org/OleTraveler/bones.svg?branch=master)](https://travis-ci.org/OleTraveler/bones)



## Credits

* "Your bones got a little machine" - Black Francis
* The API for this project is adapted from the [Joi Project](https://github.com/hapijs/joi) .
* John De Goes [Free Applicative Talk](https://www.youtube.com/watch?v=H28QqxO7Ihc)
* Kris Knuttycombe's [Xenomorph Library](https://github.com/nuttycom/xenomorph) is similar to this.
* Scodec is an amazing library.  I learned a lot from that library.







  
