# Overview

Provides the ability to create insert, update, delete and select statements 
for a given BonesSchema.  Also provides a DDL.

Essentually provides the following method:


```scala

/** select id, <values> from a where id = Long */
def get[A]: Long => Connection => Either[NonEmptyList[ExtractionError], (Long,A)]

/** update a set (<keys> = <values>) where id = long)*/
def update[A]: (Long, A) => Connection => Either[NonEmptyList[SystemError], (Long,A)]

/** delete from a where id = long */
def delete[A]: ID => Connection => Either[NonEmptyList[ExtractionError], (Long, A)]

/** insert into a (<keys>) (<values) */
def insert[A]: A => Connection => Either[SystemError, (ID,A) ]

/** select id, <values> from a */
def search[A]: Connection => Stream[IO, Either[NonEmptyList[ExtractionError], (Long,A)]]


``` 