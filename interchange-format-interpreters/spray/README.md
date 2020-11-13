# Overview

This module provides JSON Encoding and Validation using the Spray Json Library.

# Functions

This library essentially provides:

```scala
def encode[A]: A => spray.json.JsValue
def validate[A]: spray.json.JsValue => Either[ExtractionErrors,A]
```

Since Array[Byte] and String can be converted to Json, this library implicitly provides Encoding and Validations
directly to Array[Byte] and String.
```scala
def encode[A]: A => String // Where String is a Valid String encoding of a JsValue document
def encode[A]: A => Array[Byte] // Where Array[Byte] is a valid byte encoding of a JsValue document

def validate[A]: String => Either[ExtractionErrors,A] // Where String should be a String encoding of a JsValue document
def validate[A]: Array[Byte] => Either[ExtractionErrors,A] // Where Array[Byte] is a valid byte encoding of a JsValue docuemnt. 
```