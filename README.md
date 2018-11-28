# Bones

** Please note that the Bones project is currently a work in progress. **

## Purpose

The purpose of this library is to remove as much boilerplate as possible out of a CRUD/REST application.  
Some REST services (including Swagger doc) can be written in as little as 5 expressions.  
 * Define the input and output and error schema -- Using a declarative syntax, describe the expected data.
 * Define the allowable CRUD actions for a schema type. 
 * Pass the items to an interpreter.
 
 

### Schema DSL
Bones defines a Domain Specific Language (DSL) for describing CRUD applications with validation.

Using the DSL will result in creating a Generalized Algebraic Data Type (GADT) data structure.
The GADT structure describes the data, validation and available Create/Read/Update/Delete (CRUD) actions.
 
As an example, we will describe a Person CRUD application.  For a complete running service writing to the Database,
refer to the [Example](https://github.com/OleTraveler/bones/blob/master/examples/src/main/scala/com/bones/PersonEndpoint.scala
)

```$scala

import com.bones.syntax._
import com.bones.validation.ValidationDefinition.{IntValidation => iv, StringValidation => sv}

case class Person(name: String, age: Int)

val personSchema = (
  kvp("name", string(sv.matchesRegex("^[a-zA-Z ]*$".r))) ::
  kvp("age", int(iv.min(0))) ::
  KvpNil
).convert[Person]

case class Error(error: String)
  
val errorDef = (kvp("error", string) :: KvpNil).convert[Error]

val personService = ServiceOps.withPath("person")
    .withCreate(personSchema, personWithId, errorDef)
    .withRead(personWithId, errorDef)
    .withUpdate(personSchema, personWithId, errorDef)
    .withDelete(personWithId, errorDef)

```


### Interpreters

The power in creating a GADT structure, is that we can provide different interpretations of the defined Schema.
Once the basic description of the service is created, many different programs can interpret not only 
the data structure 
in different contexts, but also define implied behavior.  If implied behavior is similar between systems, then the exact
same interpreter can be used across services and the only change would be the schema and service description as described above.

I other words, if services perform similar behavior, 
such as write to Kafka, S3, a Database or an external services, the boilerplate code can be reduced by using GADTs.
This project is to provide a common vocabulary, and reasonable API, for any CRUD service.  See below for the list of interpreters.

This example uses the Scala http4s.

```$scala
val service =
  HttpInterpreter("/person")
    .withContentType(jsonFormat)
    // .withContentType(protobufFormat) Not implemented yet.

// The generated endpoint will take care of unmarshalling JSON and validating against the schema defined above.
// If successfull, will pass the Person case class to the specified function.
// Also responsible for marshalling the Error or Successful Results.
val http4Service: HttpRoutes[IO] = service.forService(
    personService,
    person => IO { Right(person) }, // create: a function from Person => IO[Either[Error,Person]] 
    id => IO { Right(Person("Janis Ian", 30, None))}, //read: a function from long => IO[Either[Error,Person]]
    (id, person) => IO { Right(person) }, //update: a function from (long,Person) => IO[Either[Error, Person]]
    id => IO { Right(Person("Janis Ian", 30, None))} // delete: a function from (long) => IO[Either[Error, Person]]
)

object PersonEndpoint extends IOApp {

  override def run(args: List[String]): IO[ExitCode] = {

    BlazeBuilder[IO].bindHttp(8080, "localhost").mountService(http4Service, "/")
        .serve
        .compile.drain.as(ExitCode.Success)
    }
}


```

We can use the same schema to generate Swagger compliant doc.  This will eventually be integrated in the
http4s interpreter.

```$scala
val openApi = new OpenAPI()

CrudOasInterpreter.jsonApiForService(Definitions.personService).apply(openApi)

println(io.swagger.v3.core.util.Json.mapper().writeValueAsString(openApi))
```
 


## Download

Version 0.5.0-SNAPSHOT includes validation, CRUD and the http4s REST Interpreter.


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







  
