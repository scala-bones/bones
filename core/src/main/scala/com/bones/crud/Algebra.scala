package com.bones.crud

import com.bones.data.Algebra.DataDefinitionOp

/**
  * Defines the algebra for the CRUD actions.
  */
object Algebra {

  def create[I, E, O](
     inputSchema: DataDefinitionOp[I],
     errorSchema: DataDefinitionOp[E],
     successSchema: DataDefinitionOp[O]
   ): Create[I,E,O] =
    Create[I,E,O](inputSchema, successSchema, errorSchema)

  def read[A](successSchema: DataDefinitionOp[A]): Read[A] = Read(successSchema)

  def update[I, E, O](
    inputSchema: DataDefinitionOp[I],
    errorSchema: DataDefinitionOp[E],
    successSchema: DataDefinitionOp[O]
  ): Update[I,E,O] = Update(inputSchema, errorSchema, successSchema)


  sealed trait CrudOp[+A]

  case class Create[I,E,O] private (
    schemaForCreate: DataDefinitionOp[I],
    successSchemaForCreate: DataDefinitionOp[O],
    errorSchemaForCreate: DataDefinitionOp[E]
  ) extends CrudOp[O]
  case class Read[O] private (successSchemaForRead: DataDefinitionOp[O]) extends CrudOp[O]

  case class Update[I,E,O] private (
    inputSchema: DataDefinitionOp[I],
    failureSchema: DataDefinitionOp[E],
    successSchema: DataDefinitionOp[O]
  ) extends CrudOp[O]

  case class Delete[O] private (
    successSchema: DataDefinitionOp[O]) extends CrudOp[O]

  case class Search()

}

