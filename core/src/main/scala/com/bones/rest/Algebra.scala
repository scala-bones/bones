package com.bones.rest

import com.bones.data.Algebra.DataDefinitionOp

object Algebra {

  sealed trait CrudOp[+A]

  trait Create[I, E, O] extends CrudOp[O] {
    def schemaForCreate: DataDefinitionOp[I]
    def successSchemaForCreate: DataDefinitionOp[O]
    def errorSchemaForCreate: DataDefinitionOp[E]
  }

  trait CreateInterpreter[A] {
    def interpretCreate[I,E,O](create: Create[I,E,O]): A
  }

  trait Read[O] {
    def successSchemaForRead: DataDefinitionOp[O]
  }

  trait ReadInterpreter[A] {
    def interpretRead[O](reader: Read[O]): A
  }

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

//  def create[I, E, O](
//                             inputSchema: DataDefinitionOp[I],
//                             successSchema: DataDefinitionOp[O],
//                             errorSchema: DataDefinitionOp[E]
//                           ): Create[I,E,O] =
//    Create[I,E,O](inputSchema, successSchema, errorSchema)
//
//  def read[A](successSchema: DataDefinitionOp[A]) = Read(successSchema)
}
