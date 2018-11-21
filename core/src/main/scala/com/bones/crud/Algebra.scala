package com.bones.crud

import com.bones.data.Value.DataClass


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
                     inputSchema: DataClass[I],
                     successSchema: DataClass[O],
                     errorSchema: DataClass[E]
   ): Create[I,O,E] =
    Create[I,O,E](inputSchema, successSchema, errorSchema)

  /**
    * Use to create a GADT describing how to read data.
    * @param successSchema The data returned on the read.
    * @tparam A the data
    * @return a GADT algebra describing read.
    */
  def read[A](successSchema: DataClass[A]): Read[A] = Read(successSchema)

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
                     inputSchema: DataClass[I],
                     successSchema: DataClass[O],
                     errorSchema: DataClass[E]
  ): Update[I,E,O] = Update(inputSchema, errorSchema, successSchema)

  def delete[O](
    outputSchema: DataClass[O]
  ): Delete[O] = Delete(outputSchema)


  case class Create[I,O,E](
                            schemaForCreate: DataClass[I],
                            successSchemaForCreate: DataClass[O],
                            errorSchemaForCreate: DataClass[E]
  ) extends CrudOp[I]

  case class Read[A](successSchemaForRead: DataClass[A]) extends CrudOp[A]

  case class Update[I,O,E](
                            inputSchema: DataClass[I],
                            successSchema: DataClass[O],
                            failureSchema: DataClass[E]
  ) extends CrudOp[I]

  case class Delete[O] (
    successSchema: DataClass[O]) extends CrudOp[O]

  case class Search()

}

