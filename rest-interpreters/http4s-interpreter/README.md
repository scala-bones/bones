# Overview

This module is used to provide Http4s routes for a schema.


Currently supported is a CRUD interface for a given Schema.

For instance, if a schema is defined as such:

```scala
case class User(name: String, age: Int)
val user = kvp("name", string) :: kvp("age", int) :: kvpNil
```

This module will provide org.http4s.HttpRoutes the following endpoints:

```
PUT /user => create new  user
GET /user/:id => get user by id
POST /user/:id => update user by id
DELETE /user/:id => delete user by id
GET /user => return all user entities.
```

This module provides the following contentType:
```
application/json
application/bson
application/protobuf
```
