# Bones

** Please note that the Bones project is currently a work in progress.  Limitations where noted (and many where not noted) **

## DSL 
Bones defines a Domain Specific Language (DSL) for describing CRUD applications with validation.

Using the DSL will result in creating a Generalized Algebraic Data Type (GADT) data structure.
The GADT structure describes the data, validation and available Create/Read/Update/Delete (CRUD) actions.

 
As an example, we will describe a CRUD application that creates and reads a Person.

```$scala

case class Person(name: String, age: Int)

val personSchema = obj2(
  key("name").string(sv.matchesRegex("^[a-zA-Z ]*$".r)),
  key("age").int(iv.min(0))
).transform[Person]

val errorDef: DataDefinitionOp[String] = StringData()

val serviceDescription = List(create(personSchema, personSchema, errorDef), read(personSchema))

```


## Interpreters

The power in creating a GADT structure, is that we can provide different interpretations of the defined Schema.
Once the basic description of the service is created, many different programs can interpret not only the data structure 
in different contexts, but also define implied behavior.  If implied behavior is similar between systems, then the exact
same interpreter can be used across services and the only change would be the schema and service description as described above.

I other words, if services perform similar behavior, 
such as write to Kafka, S3, a DB or an external services, the boilerplate code can be reduced by using GADTs.
This project is to provide a common vocabulary, and reasonable API, specifically for CRUD services.

The following 3 interpreters work together to produce an actual REST web service, it's swagger documentation 
and the postgres schema to support the REST web service.  


### REST / JSON / Doobie DB Interpreter

** __Note:__ only GET and POST are currently supported **

The __Direct to Doobie__ interpreter will spin up a Jetty container with an application which will listen
for GET (based on the _read()_ definition) and POST (based on the _create()_ definition).  
By convention, the resulting interpreted application will expect a JSON object which will then be validated based on the schema.
If validation is successful, we will issue an insert statement for the POST and a select statement for the GET request.
By convention, the insert will use the DB specific auto-generated id and therefor it is implied that the 
resulting JSON will also include an id property.

To enable the Direct to Doobie service, we need to define queries specific to our case class
as well as configure the name of the endpoint.

```$scala

//the next bit of code are the Queries needed to select and insert data using Doobie.
//(Currently unsure of how to auto generate these queries, but it should be possible)  
val doobieInfo = new DoobieInfo[Person] with EndPoint {
    override def transactor: Aux[IO, Unit] = Transactor.fromDriverManager[IO](
      "org.postgresql.Driver", "jdbc:postgresql:bones", "postgres", ""
    )
    
    override def get(id: Long): doobie.ConnectionIO[Person] =
      sql"select name, age from person where id = $id".query[Person].unique
    
    override def insert(a: Person): doobie.ConnectionIO[Int] = {
      val name = a.name
      val age = a.age
      sql"insert into person (name, age) values ($name, $age)".update.run
    }

        
    override def url: String = "/person"
}
```

Finally, given that we have a schema and the DoobieInfo for the case class, we 
can start the service.  For this interpreter we are using the Unfiltered library.

```$scala
//this will create the URL Paths, 
//in this case the GET and POST will be available at https://localhost:5678/person will be available.
val restToDoobieInterpreter = DirectToDoobie("/person")

val serviceDefinitions = restToDoobieInterpreter(serviceDescription)(doobieInfo)

//To start the daemon, we can pass the endpoint serviceDefinitions to Jetty.
jetty.Http(5678).filter(new Planify(plan)).run
```


### Swagger Interpreter

** __Note:__ -- Work in progress. Currently incomplete. ** 

The __swagger__ interpreter will generate a JSON document describing the REST data structure in Swagger format.
This can be used in conjunction with a REST Interpreter to produce documentation describing the REST endpoint.
Note that we can reuse the same person schema DataDefinition that we pass to the REST interpreter.  Therefor any changes
to the data schema will be reflected in the Swagger doc automatically.

```$scala
   // swaggerDoc now contains an OAS compliant string describing the REST endpoint.  Note that we are reusing 
   // the same serviceDescription as the REST interpreter.
   val swaggerDoc = CrudOasInterpreter().apply(serviceDescription).apply("/person", "/people").spaces2
```

### Postgres Schema Generator

** __Note:__ -- Work In Progress. Results may vary.  Please double check that the output is appropriate.**  

The __postgres schema generator__ interpreter will create a string which can be used to create the table in postgres which
corresponds to how the database schema needs to exists in order to support the _REST / JSON / Doobie DB Interpreter_.

This generator currently implies creating an `id` column that is auto incremented by the database.
```$scala
//the results will be something along the lines of `create table person (id serial, name text, age int8)`
val psqlSchema = DoobiePostgresSchema("person").apply(serviceDescription)
```

This runnable example can be seen in the 
[Person Endpoint](https://github.com/OleTraveler/bones/blob/master/examples/src/main/scala/com/bones/PersonEndpoint.scala).




## Download

Version 0.4.0 has JSON validation, but lacks the full CRUD definitions.
Version 0.5.0-SNAPSHOT includes validation, CRUD and a partial REST Interpreter.


### Stableish
```libraryDependencies += "com.github.oletraveler" %% "bones" % "0.4.0"```

Interpreters (no need to include core if you include this)
#### Lift Json Interpreter (currently the only interpreter)
```libraryDependencies += "com.github.oletraveler" %% "bones-rest-unfiltered" % "0.4.0"```

### Snapshot
```libraryDependencies += "com.github.oletraveler" %% "bones" % "0.5.0-SNAPSHOT"```

Interpreters (no need to include core if you include this)
#### Lift Json Interpreter (currently the only interpreter)
```libraryDependencies += "com.github.oletraveler" %% "bones-rest-unfiltered" % "0.5.0-SNAPSHOT"```


## Credits

* "Your bones got a little machine" - Black Francis
* The API for this project is adapted from the [Joi Project](https://github.com/hapijs/joi) .
* John De Goes [Free Applicative Talk](https://www.youtube.com/watch?v=H28QqxO7Ihc)
* Kris Knuttycombe's [Xenomorph Library](https://github.com/nuttycom/xenomorph) is similar to this.
* Scodec is an amazing library.  I learned a lot from that library.
