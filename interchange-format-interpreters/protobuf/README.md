# Overview

This module provides Protocol Buffers Encoding and Validation using the [Java Protobuf](https://developers.google.com/protocol-buffers) Library.

# Functions

This library essentially provides encoding and validating Values to and from Array[Byte] 
utilizing the Protbuf CodedInputStream.

```scala
def encode[A]: A => Array[Byte]
def validate[A]: Array[Byte] => Either[ExtractionError,A]
```
