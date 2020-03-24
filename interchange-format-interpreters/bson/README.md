# Overview

This module provides BSON (Binary Json) Encoding and Validation using the Scala [Reactive Mongo](http://reactivemongo.org/) Library.

# Functions

This library essentially provides:

```scala
def encode[A]: A => reactivemongo.bson.BSONValue
def validate[A]: reactivemongo.bson.BSONValue => Either[ExtractionError,A]
```

Since Array[Byte] and String can be converted to Json, this library implicitly provides Encoding and Validations
directly to Array[Byte] and String.
```scala
def encode[A]: A => Array[Byte] // Where Array[Byte] is a valid byte encoding of a BSON document

def validate[A]: Array[Byte] => Either[ExtractionError,A] // Where Array[Byte] is a valid byte encoding of a BSON docuemnt. 
```



# Limitations

BSONDateTime only supports up to millisecond.  Therefor when we serialize/deserialize a LocalDateTime, we will
loose some significance.

TODO validators need to validate

