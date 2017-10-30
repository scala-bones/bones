# Overview

The idea behind Soy is to create simple Joi like validation simple, but also make more complex
validations possible.

Also using Free Applicative to create an AST describing the validation.  The AST can then be passed to a compiler to produce the output or use the same AST to produce documentation.


## Getting Started

This example is adapted from the [https://github.com/hapijs/joi JOI Proejct].

```$scala
    val rvp: JsonProducer = ???

    case class User(username: String, pass: String, message: Option[String], role: Option[String])
    
    val prog = Soyo.obj4(
      key("username").string.alphanum(),
      key("password").string(),
      key("message").string().optional(),
      key("role").string().valid("manager", "employee").optional(),
      User(_,_,_,_)
    )

    import cats.implicits._
    val user = prog.foldMap[FromProducer](defaultCompiler).apply(rvp)

```
