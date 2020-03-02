# Bones

** Please note that the Bones project is currently a work in progress. **
However, the core library is now essentially feature complete for what will eventually be 
version 1.0.  I would love to get some feedback.

One of the simplest ways to get started is to generate your test data
by defining a Bones Schema and using the Scalacheck Interpreter to produce an
Arbitrary[A] where A is your class which is to be generated with data that conforms to the 
Schema definition.

See a [Scalacheck Example](https://github.com/OleTraveler/bones/blob/master/test-interpreters/scalacheck/src/test/scala/com/bones/scalacheck/ScalacheckExample.scala)


## Purpose

The purpose of this library is to remove boilerplate for anything which processes Data and requires validation.
 
The current targets are:
 * Generate Arbitrary Test data based on a schema.
 * Validate interchange format data (such as Json, Protobuf) based on a schema.
 * Remove boilerplate for Data Conversion in a REST Application.
 * Remove boilerplate for Generating REST specifications (such as OpenAPI format). 
 
 
## Data Types.

### Native Types

Bones currently supports the following data types natively.

```
String, Int, Boolean, Either, Long, List, Short, Float, Double, BigDecimal, Byte Array, LocalDateTime,
LocalDate, UUID, Enumeration
```

### Custom Types
Custom types allow a user to define their own Algebra.  There are a few use cases for this.

Custom types allow us to create Algebra for types not supported natively above.  For instance, there is a
package com.bones.data.custom.JavaTimeValue which contains an Algebra to support types found
in the java.time package.  This allows us to provide these types for our core interpreters without
forcing them to be supported in all custom interpreters.

Custom types allow for a user to create types that are context specific.  For instance, a
type of `Markdown` would be a string that, when used in the context of a web application, would display a
markdown editor for the user to enter in data.  And in the context of a JSON API, would validate that the
markdown is valid.

Custom types allow a project or an organization to cherry-pick custom algebras and context specific algabras
for a standard project/organization wide vocabulary.

### Product Types / Sum Types
Product Types (Case classes / Shapeless HList) and Sum Types (traits/case classes or 
Shapeless Coproducts) are also supported for collections of both Native types and Custom types.


  

### Schema DSL
Bones defines a Domain Specific Language (DSL) for describing CRUD applications with validation.

Using the DSL will result in a Generalized Algebraic Data Type (GADT) tree-like data structure,
where each leaf node represent a single data type and internal nodes represent a collection of data types. 
 
As an example, we will describe a Person CRUD application.  For a complete running service writing to the Database,
refer to the [Example](https://github.com/OleTraveler/bones/blob/master/examples/http4s-examples/src/main/scala/com/bones/PersonEndpoint.scala
)

```$scala

import com.bones.syntax._

  case class Person(name: String, age: Int, gender: Option[String])

  val personSchema = (
    ("name", string(sv.matchesRegex("^[a-zA-Z ]*$".r)), "The name of the person must be alphanumeric", "John Doe") :<:
      ("age", int(iv.min(0)), "The Age, In years, of the person.", 21) :<:
      ("gender", string.optional) :<:
      kvpNil
  ).convert[Person]

case class Error(error: String)
  
```


### Interpreters

The power in creating a GADT data structure, is that we can feed it as input to different interpreters.
Once the basic functionality of the interpreter is created, we can reuse the interpreter passing in different schemas
to create common functionality.

In other words, if services perform similar behavior, 
such as validate HTTP requests and write to Kafka, S3, a Database or an external services, the boilerplate code can be reduced by using GADTs.
This project is to provide a common vocabulary for any service, however this project is currently focused on 
REST CRUD apps.
 
This example uses the Scala http4s with schema defined above and for less than 11 LINES OF CODE, provides full CRUD functionality writing to 
a JDBC datasource.  It provides 3 interchange formats: JSON, BSON and Protobuf (yes, protobuf!), 
and finally a protofile describing the data and OAS3 compliant Schema.  
The idea is that this will work for any Bones Schema Values.


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

Serializer / Marshaller Interpreters
* [Circe](interchange-format-interpreters/circe/README.md)
* [BSON](interchange-format-interpreters/bson/README.md)
* [Argonaut](interchange-format-interpreters/argonaut/README.md)
* [protobuf](interchange-format-interpreters/protobuf/README.md)
* [scalacheck](test-interpreters/scalacheck/README.md)

HTTP REST Interpreters
* [http4s](rest-interpreters/http4s-interpreter/README.md)

Incomplete Interpreters
* [swagger](interchange-format-interpreters/swagger-oas3/README.md) - Does not currently support Coproduc (FIXED!!! in master, not released)
* [dbJdbc](db-interpreters/jdbc/README.md) - Does not currently support coproducts
* [React](client-interpreters/react/README.md) - This is more of a POC.  Does not currently support Coproducts
* [http4s-client](client-interpreters/http4s-client/README.md) - Not Implemented 


## Download

Version 0.5.0 includes validation, CRUD and the http4s REST Interpreter.


#### Getting Started with Http4s Circe Interpreter
Basic CRUD capabilities implemented with http4s, circe and JDBC.

```libraryDependencies += "com.github.oletraveler" %% "examples" % "0.5.0"```

Then check out the [LocalhostAll Example](examples/http4s-examples/src/main/scala/com/bones/fullstack/LocalhostAll.scala)
and the 

### CI
[![Build Status](https://travis-ci.org/OleTraveler/bones.svg?branch=master)](https://travis-ci.org/OleTraveler/bones)



## Credits

* "Your bones got a little machine" - Black Francis
* The API for this project is adapted from the [Joi Project](https://github.com/hapijs/joi) .
* John De Goes [Free Applicative Talk](https://www.youtube.com/watch?v=H28QqxO7Ihc)
* Kris Knuttycombe's [Xenomorph Library](https://github.com/nuttycom/xenomorph) is similar to this.
* Scodec is an amazing library.  I learned a lot from that library.







  
