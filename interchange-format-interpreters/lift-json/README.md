# Overview

This module provides JSON Encoding and Validation using the [Lift JSON](https://github.com/lift/lift) Library.

# Functions

This library essentially provides:

```scala
def encode[A]: A => net.liftweb.json.JsonAST
def validate[A]: net.liftweb.json.JsonAST => Either[ExtractionError,A]
```

Since Array[Byte] and String can be converted to Json, this library implicitly provides Encoding and Validations
directly to Array[Byte] and String.
```scala
def encode[A]: A => String // Where String is a Valid String encoding of a Json document
def encode[A]: A => Array[Byte] // Where Array[Byte] is a valid byte encoding of a Json document

def validate[A]: String => Either[ExtractionError,A] // Where String should be a String encoding of a Json document
def validate[A]: Array[Byte] => Either[ExtractionError,A] // Where Array[Byte] is a valid byte encoding of a Json docuemnt. 
```