# Overview

This module provides the ability to describe data in a Swagger (OAS3) compliant manner
using the Java [Swagger-Core](https://github.com/swagger-api/swagger-core) library.

This module essentially provides

```scala
val f: io.swagger.v3.oas.models.media.Schema[_] => io.swagger.v3.oas.models.media.Schema[_]
```

where the function adds metadata values to the Schema.  Note that Schema is a Java class that is mutable,
and so the same Schema[_] object is returned.