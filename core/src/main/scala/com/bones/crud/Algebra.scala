package com.bones.crud

import com.bones.data.Value.Value


/**
  * Defines the algebra for the CRUD actions.
  */
object Algebra {

  sealed trait CrudOp[A]



  /**
    *
    * @param inputSchema The expected schema to create new data.
    * @param errorSchema The schema for the error.
    * @param successSchema The for the returned value.
    * @return a GADT describing create
    */
  def create[I,O,E](
    inputSchema: Value[I],
    successSchema: Value[O],
    errorSchema: Value[E]
   ): Create[I,O,E] =
    Create[I,O,E](inputSchema, successSchema, errorSchema)

  /**
    * Use to create a GADT describing how to read data.
    * @param successSchema The data returned on the read.
    * @tparam A the data
    * @return a GADT algebra describing read.
    */
  def read[A](successSchema: Value[A]): Read[A] = Read(successSchema)

  /**
    * Used to create a GADT describing how to upadate data.
    * @param inputSchema The data the CRUD app is expecting to receive.
    * @param errorSchema The data the CRUD app returns on error.
    * @param successSchema The data the CRUD app returnes on Success.
    * @tparam I Input type.
    * @tparam E Error Type
    * @tparam O Output Type
    * @return a GADT algebra describing update.
    */
  def update[I,O,E](
    inputSchema: Value[I],
    successSchema: Value[O],
    errorSchema: Value[E]
  ): Update[I,E,O] = Update(inputSchema, errorSchema, successSchema)

  def delete[O](
    outputSchema: Value[O]
  ): Delete[O] = Delete(outputSchema)


  case class Create[I,O,E](
    schemaForCreate: Value[I],
    successSchemaForCreate: Value[O],
    errorSchemaForCreate: Value[E]
  ) extends CrudOp[I]

  case class Read[A](successSchemaForRead: Value[A]) extends CrudOp[A]

  case class Update[I,O,E](
    inputSchema: Value[I],
    successSchema: Value[O],
    failureSchema: Value[E]
  ) extends CrudOp[O]

  case class Delete[O] (
    successSchema: Value[O]) extends CrudOp[O]

  case class Search()

}

