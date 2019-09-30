# Overview

TODO: TEST!!!

The purpose of this module is to encode values directly to various data types (String, Array[Byte]) 
without using a 3rd party parser.  This should be the fastest, most lightweight, 
and least featured encoders.

# Currently Support

## Value to String encoded Json

This essentually provides:
```scala
def f[A]: A => String
```