# EitherData

Either Data is currently defined as such:

```scala
final case class EitherData[ALG[_], A: Manifest, B: Manifest](
  definitionA: Either[HigherOrderValue[ALG, A], ALG[A]],
  definitionB: Either[HigherOrderValue[ALG, B], ALG[B]])
    extends HigherOrderValue[ALG, Either[A, B]] {
  val manifestOfLeft: Manifest[A] = manifest[A]
  val manifestOfRight: Manifest[B] = manifest[B]
}
```

This is saying that the data that is represented can be one of two
types, an A type or a B type.  Additionally, each type A/B can be 
a higher order type or a basic type.  A higher order type can 
contain a primitive or can be a sub-collection
which allows for hierarchical data.  This allows for some flexibility,
but also leads to some inconsistent data representation across formats.


## Left == Primitive && Right == Primitive

As an example, we allow for the age to be an int or a numeric string.
```scala
("age", either(int(iv.positive), str(sv.numeric)) )
```

### JSON

The JSON representation will be one element using the key as the name.  
This is true even if the data types are different because
JSON's nature of being non-strict.

 Left Value
```json
  {  "age": 5 }
```
Right Value
```json
  {  "age":  "5" }
```

In order to parse this data, Either Data first passes priority to the Left value.  If the parser
returns any error, then priority passes to the Right value parser.  If the
right parser successfully parses the data, then the CanNotConvert error, returned from the right parser,
is discarded.



### Protobuf
The Protobuf representation will be 2 different field numbers using a 'oneof' keyword.

```proto
message SampleMessage {
  oneof key_oneof {
    int32 age1 = 1;
    string age2 = 2;
  }
}

```

As a future enhancement, it might be beneficial to detect if both fields can be represented by
the same type and use a single field.

### Relational Database

The database representation will be 2 different columns.

```sql
  create table sample (age_int int8, age_string text)
```

| age_int | age_string |
|---------|------------|
|5        |5           |


## Left == Primitive && Right == Collection

For example, if we had a length field where we accept metric in meters or imperial units
in feet and inches.
```scala
  val imperial = ("feet", int()) :: ("inches", double()) :: kvpNil
  val len = ("length", either(double(), imperial)) :: kvpNil
```


### Json
The JSON representation will be one element using the key as the name.  
This is true even if the data types are different because
JSON's nature of being non-strict.

```json
{ "length" : 0.9144 }
```
```json
{ "length" : { "feet" : 3, "inches": 0 }
```



