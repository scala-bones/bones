# AST

There are a few types of 

### Primitive Values
Primitive values are the building blocks of a schema.  Not only can
primitive values be types such as Int, Timestamp, etc, but they
can also be any type that represents a collection of data such as 
Tuples of Primitive Values, classes, coproducts or even HLists.

### Concrete Value
A concrete value is a primitive value and therefor can recursively contain itself. 
manifestation of a collection of one or key value pairs.

A concrete value is meant to be the 'visible' data type of a schema.

When creating an interpreter for concrete values, it is sometimes necessary
to create one interpreter when a concrete value is the top-level value
and one when the concrete value is a nested value.  This is because
the nested value has a name, while the top-level value does not.


### KvpCollection
KvpCollection represents a collection of key value pairs.  Currently, 
the key is always a string type, and the values are represented as an HList.
The HList can contain any types of values such as primitives 
(such Int, String, Timestamp), product types (case classes, tuples and even sub-hlists),
and coproduct types (abstract classes, traits and shapeless coproduct types).





