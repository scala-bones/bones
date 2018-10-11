package com.bones.crud

import com.bones.data.Value.ValueDefinitionOp


/**
  * Defines the algebra for the CRUD actions.
  */
object Algebra {

  sealed trait CrudOp[+A]



  /**
    *
    * @param inputSchema The expected schema to create new data.
    * @param errorSchema The schema for the error.
    * @param successSchema The for the returned value.
    * @tparam I Input type
    * @tparam E Error type
    * @tparam O Output type
    * @return a GADT describing create
    */
  def create[I, E, O](
     inputSchema: ValueDefinitionOp[I],
     errorSchema: ValueDefinitionOp[E],
     successSchema: ValueDefinitionOp[O]
   ): Create[I,E,O] =
    Create[I,E,O](inputSchema, successSchema, errorSchema)

  /**
    * Use to create a GADT describing how to read data.
    * @param successSchema The data returned on the read.
    * @tparam A the data
    * @return a GADT algebra describing read.
    */
  def read[A](successSchema: ValueDefinitionOp[A]): Read[A] = Read(successSchema)

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
  def update[I, E, O](
    inputSchema: ValueDefinitionOp[I],
    errorSchema: ValueDefinitionOp[E],
    successSchema: ValueDefinitionOp[O]
  ): Update[I,E,O] = Update(inputSchema, errorSchema, successSchema)

  def delete[O](
    outputSchema: ValueDefinitionOp[O]
  ): Delete[O] = Delete(outputSchema)


  case class Create[I,E,O](
    schemaForCreate: ValueDefinitionOp[I],
    successSchemaForCreate: ValueDefinitionOp[O],
    errorSchemaForCreate: ValueDefinitionOp[E]
  ) extends CrudOp[O]

  case class Read[O](successSchemaForRead: ValueDefinitionOp[O]) extends CrudOp[O]

  case class Update[I,E,O](
    inputSchema: ValueDefinitionOp[I],
    failureSchema: ValueDefinitionOp[E],
    successSchema: ValueDefinitionOp[O]
  ) extends CrudOp[O]

  case class Delete[O] (
    successSchema: ValueDefinitionOp[O]) extends CrudOp[O]

  case class Search()

}

