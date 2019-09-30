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

Product Types (Tuples) and Sum Types (traits/case classes) are also support.

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
 
This example uses the Scala http4s with schema defined above and for less than 40 LINES OF CODE, provides full CRUD functionality writing to 
a JDBC datasource.  It provides 3 interchange formats: JSON, BSON and Protobuf (yes, protobuf!), 
a protofile describing the data and OAS3 compliant Schema.


```$scala
object PersonEndpoint extends LocalhostAllIOApp {

  import LocalhostAllIOApp._

  val ds: HikariDataSource = localhostDataSource
  
  case class BasicError(message: String)
  private val basicErrorSchema =
    (kvp("message", com.bones.syntax.string) :: KvpNil).convert[BasicError]  

  override def services: HttpRoutes[IO] = {
    val path = "person"
    
    
    val middleware = CrudDbDefinitions(schema, ds)


    val interpreter = ClassicCrudInterpreter.allVerbs[A,BasicError,IO,Long](
      path,
      StandardCharsets.UTF_8,
      personSchema,
      kvp("id", long(lv.min(1))),
      str => Try(str.toLong).toEither.left.map(ex => BasicError("Could not convert parameter to a Long value")),
      basicErrorSchema,
      Kleisli(middleware.createF).map(_.left.map(e => extractionErrorToBasicError(e))).run,
      Kleisli(middleware.readF).map(_.left.map(e => extractionErrorsToBasicError(e))).run,
      Function.untupled(Kleisli(middleware.updateF.tupled).map(_.left.map(e => extractionErrorsToBasicError(e))).run),
      Kleisli(middleware.deleteF).map(_.left.map(e => extractionErrorsToBasicError(e))).run,
      () => middleware.searchF
    )

    val dbRoutes = dbSchemaEndpoint(path, schema)

    interpreter.createRoutes <+> dbRoutes  
  }
    
}


## Download

Version 0.5.0 includes validation, CRUD and the http4s REST Interpreter.


#### Http4s Circe Interpreter (currently the only interpreter)
Basic CRUD capabilities implemented with http4s, circe and doobie.
```libraryDependencies += "com.github.oletraveler" %% "bones-http4s-circe" % "0.5.0-SNAPSHOT"```

### CI
[![Build Status](https://travis-ci.org/OleTraveler/bones.svg?branch=master)](https://travis-ci.org/OleTraveler/bones)



## Credits

* "Your bones got a little machine" - Black Francis
* The API for this project is adapted from the [Joi Project](https://github.com/hapijs/joi) .
* John De Goes [Free Applicative Talk](https://www.youtube.com/watch?v=H28QqxO7Ihc)
* Kris Knuttycombe's [Xenomorph Library](https://github.com/nuttycom/xenomorph) is similar to this.
* Scodec is an amazing library.  I learned a lot from that library.







  
