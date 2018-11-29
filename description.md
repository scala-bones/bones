I'm working on a project meant to reduce the boilerplate for REST/CRUD applications.

First, using a DSL, one describes the data they are expecting to receive/return.  For instance:

```
case class Person(name: String, age: Int)
val personSchema = (
  kvp("name", string(sv.matchesRegex("\^\[a-zA-Z \]\*$".r))) ::
  kvp("age", int(iv.min(0))) ::
  KvpNil
).convert\[Person\]

case class Error(error: String)
val errorDef = (kvp("error", string) :: KvpNil).convert\[Error\]
```

Next we describe the available operations:  
```
 val personService = ServiceOps.withPath("person")
   .withCreate(personSchema, personWithId, errorDef)
   .withRead(personWithId, errorDef)
   .withUpdate(personSchema, personWithId, errorDef)
   .withDelete(personWithId, errorDef)
```

The service description is the schema which can be passed to different interpreters.  For instance the OpenAPI interpreter will print OpenAPI/Swagger compliant documentation of the service base on the schema.

```
val openApi = new OpenAPI() 
CrudOasInterpreter.jsonApiForService(personService).apply(openApi)
println(io.swagger.v3.core.util.Json.mapper().writeValueAsString(openApi)
```

The http4s interpreter will generate HttpRoutes\[IO\] for each of the defined operations.  The interpreter is responsible 
for generating a runtime which will unmarshal,validate and marhsall based on the schema.
```
      val http4Service = service.forService(
        personService,
        middleware.createF,
        middleware.readF,
        middleware.updateF,
        middleware.deleteF
      )

      BlazeBuilder[IO].bindHttp(8080, "localhost").mountService(http4Service, "/")
        .serve
        .compile.drain.as(ExitCode.Success)
```

Here is the complete [Example](https://github.com/OleTraveler/bones/blob/master/examples/src/main/scala/com/bones/PersonEndpoint.scala).

I like this approach because updates to the schema will be reflected in each interpreter.
(The doc stays up to date with the application!)

The next interpreter I'm interested in writing is one which can marhsall/unmarshall protobuf based on the schema.
It will also be able to generate a .proto definition.  This would be integrated in the http4s 
interpreter so that a config switch would generate endpoints which could accept protobuf.

I'd also like to experiment with Scala.js to see if a reasonable React application
can be generated using the schema which would be compatible with the personService.

