# Grouping

# Statement of the Problem

There are at least 3 ways for a specific group of data to be included
in a larger data set.  Each group can be expressed using the same data type.
How do we deal uniformly with each example.

Current solution is that we have a function: 
```scala 
    (TableCollection, Option[ColumnName], Option[Description]) => 
        Either[InvalidDataStructure, TableDescription]
```
If column name is not present in some situations, we return InvalidDataStructure.
The issue with this approach is that we have to deal with the case when ColumnName is not available.

# 

There are three types of grouping to deal with, each is shown in the example below.
We are using the example of the id/name pair and we'll call that group the "Id/Name"
group which can be reflected in a Scala case class as:

```scala
    case class IdName(id: Long, name: String)
```


```json
{
   "id" : 1,
   "name": "One",
   
   "child1" : {
       "id" : 1,
       "name": "One"   
   }

   "child2" : {
       "id" : 1,
       "name": "One",
       "otherData": "Y"   
   }

}
```

So the id/name pair at the root is not associated with a 'key'.  The key (aka name) is
implied with the name of the JSON type.  In this case above, we haven't defined
one as it is not in the description of the data.

Next, we have the id/name pair which is associated to the 'child1' key.  In this case,
we can assume that child1 is the name for the id/name data.

Finally, we have an id/name pair which is only a part of the data type which also includes a field
called 'otherData'.

A fully explicit data type in scala my look something like this:


```scala
    case class IdName(id: Long, name: String)
    case class MyData(idName: IdName, child1: IdName, child2: (IdName, String))
``` 

A flat database structure may look like this.
```sql
create table my_data
    (id bigint, 
    name text, 
    child1_id bigint, 
    child1_name bigint, 
    child2_id bigint, 
    child2_name text, 
    child2_other_data text
)
```

Whereas a normalized data structure may look like this.

```
create table my_data (id bigint, name);
create table child1 (id bigint, name text, my_data_id bigint)
create table child2 (id bigint, name text, other_data text, my_data_id bigint)
```


Notes:

* Instance name should be kept separate from class name.


# Next Attempt

There should be two different way to handle data, one when we are creating a
new table and one where we are adding columns to a table.  Each approach, however,
should expect a name.

# Next Problem

Running into problems when a ConcreteValue is part of a collection,
so that one more Values are associated with a single key.

For instance lets say we have the data structure:

``scala
case class IdName(id: Long, name: String) 
type SomeType = (Option[String],IdName) 

type SomeType = (Option[String], Option[IdName])

``

These are both ConcreteValue types, however they different in 
behavior because IdName will have a key for each of it's members,
whereas optional will not.  However they are of similar types
as a schema.

```scala
  type ALG[_] = Any
  val optionalString: ConcreteValue[ALG, Option[String]] = ???
  val optionalIdName: ConcreteValue[ALG, Option[IdName]] = ???
```


```json

{
  "key" : {
    "optionalString" : "X"
    "id" :  1,
    "name": "bonk"
  }
}

```

# Next Attempt
Optional should only hold ConcreteType data structures.
Add a new ConcrteType data strucutre (maybe identity)
which represents a PrimitiveValue.  The identity should
be responsible for it's own key.

What about either.  Should we allow the following definition of key
to work for  ```scala Either[Int,String]```

```json
{
 "val1": {
    "key" : 1
}

 "val2": {
    "key": "two"
  }
}
```




```



