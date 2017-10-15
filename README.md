# Overview

The idea behind Soy is to create simple Joi like validation simple, but also make more complex
validations possible.

This is also based on Scodec and combinators.

## Getting Started

This example is adapted from the [https://github.com/hapijs/joi JOI Proejct].

```$scala
val json = parse(XXX)
case class User(username: String, password: String, accessToken: Either[String, Int], birthyear: Int, email: String)

val schema = Soy.keys({
    "username".soy.string().alphanum().min(3).max(30).required(),
    "password".soy.string().regex(/^[a-zA-Z0-9]{3,30}$/),
    "access_token".soy.either(Soy.string(), Soy.int()),
    "birthyear".soy.number().integer().min(1900).max(2013),
    "email".string().email()
})

json.extract(schema).to(User) //returns Either[ValidationError,User]

```