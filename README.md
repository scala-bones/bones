# Overview


The idea behind Bones is to create a declarative syntax to define a data model.  Using the syntax will result
in a structure based on Generalized Abstract Data Types (GADT).  Using the Cats Free Applicative, we can produce 
different interpretations, or compilers as Cats calls them, of the GADT.


Currently, these are the compilers I believe are possible.
* Validation/Schema Compiler (In Progress) - Attempt to extract the data definition from Json.
* Documentation (In Progress)- Should be able to document the data requirements for
* Rest API (In My Head)- Should be able to create a REST endpoint
* JavaScript/HTML (In My Head) - Should be able to generate a GUI for data entry.
* Also, hoping to be able to integrate a DB layer.

At the end of the day, if we create compilers for each the pipeline, we should be able to generate
new services just by defining a new AST.  Granted, there will be custom steps in most pipelines,
so we will create sensible defaults that can be overwritten as well as clear extension points. 

This API for this project is adapted from the [https://github.com/hapijs/joi JOI Project].


## Getting Started

See the [Example Test](src/test/scala/com/ot/bones/ExampleTest.scala)

```$scala
    //An facade pattern on top of any JSON libary.  
    val rvp: JsonProducer = ???

    //Create the AST
    val prog = Obj.obj4(
      key("username").string().alphanum(),
      key("password").string(),
      key("message").string().optional(),
      key("location").obj2(
        key("countryIso").string(),
        key("postalCode").string().optional(),
        Location(_:String,_:Option[String])
      ).optional(),
      User(_:String,_:String,_:Option[String],_:Option[Location]) //Type annotations required for scala 2.11
    ).lift

    import cats.implicits._
    //create the program that is responsible for converting JSON into a User.
    val jsonToUserProgram = prog.foldMap[FromProducer](defaultCompiler)

    //Here we run the program by giving
    val user = jsonToUserProgram.apply(rvp)

    assert( user == Valid( User("Thisisusername", "thisispassword", None, Some(Location("US", Some("28791"))))) )


```
