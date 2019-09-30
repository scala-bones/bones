# Overview

This module provides JSON Encoding and Validation using the [Argonaut](http://argonaut.io/) Library.

# Functions

This library essentially provides:

```scala
def encode[A]: A => argonaut.Json
def validate[A]: argonaut.Json => Either[ExtractionError,A]
```

Since Array[Byte] and String can be converted to Json, this library implicitly provides Encoding and Validations
directly to Array[Byte] and String.
```scala
def encode[A]: A => String // Where String is a Valid String encoding of a Json document
def encode[A]: A => Array[Byte] // Where Array[Byte] is a valid byte encoding of a Json document

def validate[A]: String => Either[ExtractionError,A] // Where String should be a String encoding of a Json document
def validate[A]: Array[Byte] => Either[ExtractionError,A] // Where Array[Byte] is a valid byte encoding of a Json docuemnt. 
```