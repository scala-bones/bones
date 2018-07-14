package com.bones.rest

import com.bones.data.Algebra.DataDefinitionOp

object Algebra {

  sealed trait CrudOp[+A]

  case class Create[I,E,O](
    schemaForCreate: DataDefinitionOp[I],
    successSchemaForCreate: DataDefinitionOp[O],
    errorSchemaForCreate: DataDefinitionOp[E]
  ) extends CrudOp[O]
  case class Read[O](successSchemaForRead: DataDefinitionOp[O]) extends CrudOp[O]

  case class Update[I,E,R](schema: DataDefinitionOp[I],
                           //                      processor: Processor[A, F, B, E],
                           successSchema: DataDefinitionOp[R],
                           failureSchema: DataDefinitionOp[E]) extends CrudOp[R]

  case class Delete[I,E,R](processDelete: I => Either[E,R],
                           successSchema: DataDefinitionOp[R]) extends CrudOp[R]

  case class Load[T:Manifest,E]()

  case class Search()

}

object Sugar {

  import Algebra._

  def create[I, E, O](
   inputSchema: DataDefinitionOp[I],
   successSchema: DataDefinitionOp[O],
   errorSchema: DataDefinitionOp[E]
  ): Create[I,E,O] =
    Create[I,E,O](inputSchema, successSchema, errorSchema)

  def read[A](successSchema: DataDefinitionOp[A]) = Read(successSchema)
}
