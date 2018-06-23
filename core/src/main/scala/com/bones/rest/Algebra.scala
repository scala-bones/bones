package com.bones.rest

import com.bones.data.Algebra.DataDefinitionOp

object Algebra {

  case class
  EndPoint[A](url: String, actions: List[RestOp[A]]) {

    def post[I, E](
        inputSchema: DataDefinitionOp[I],
        processor: Processor[I,E,A],
        successSchema: DataDefinitionOp[A],
        errorSchema: DataDefinitionOp[E]
    ): EndPoint[A] =
      EndPoint[A](url, Create[I,E,A](inputSchema, processor, successSchema, errorSchema) :: actions)

    def get[B <: A: Manifest](successSchema: DataDefinitionOp[B]) = EndPoint(url, Read(successSchema) :: actions)

    def put[A, E[F[_]]] = ???
  }

  trait RestOp[+A]
  trait Processor[I,E,R]

  case class Create[I, E, R](
      schema: DataDefinitionOp[I],
      processor: Processor[I,E,R],
      successSchema: DataDefinitionOp[R],
      errorSchema: DataDefinitionOp[E]
  ) extends RestOp[R]

  case class Read[A:Manifest](successSchema: DataDefinitionOp[A]) extends RestOp[A]

  case class Update[I,E,R](schema: DataDefinitionOp[I],
                           //                      processor: Processor[A, F, B, E],
                           successSchema: DataDefinitionOp[R],
                           failureSchema: DataDefinitionOp[E]) extends RestOp[R]

  case class Delete[I,E,R](delete: ProcessDelete[I,E,R],
                           successSchema: DataDefinitionOp[R]) extends RestOp[R]

//  case class Processor[F[_], B, E](f: F[Either[E, B]])

  case class ProcessDelete[I, E, R]()

  case class Load[T:Manifest,E]()

  case class Search()

}

object Sugar {

  import Algebra._

  def endPoint[A](url: String): EndPoint[A] = EndPoint(url, List.empty)

}
