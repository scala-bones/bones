package com.bones.rest

import com.bones.data.Algebra.DataDefinitionOp

object Algebra {

  case class EndPoint[A:Manifest](url: String, actions: List[RestOp[A]]) {

    def post[I:Manifest, E](
        inputSchema: DataDefinitionOp[I],
        successSchema: DataDefinitionOp[A],
        errorSchema: DataDefinitionOp[E]
    ): EndPoint[A] =
      EndPoint[A](url, Create[I,E,A](inputSchema, successSchema, errorSchema) :: actions)

    def get[A: Manifest](successSchema: DataDefinitionOp[A]) = EndPoint(url, Read(successSchema) :: actions)

    def put[A, E[F[_]]] = ???
  }

  trait RestOp[+A]

  case class Create[I:Manifest, E, R](
      schema: DataDefinitionOp[I],
      successSchema: DataDefinitionOp[R],
      errorSchema: DataDefinitionOp[E]
  ) extends RestOp[R]

  case class Read[A:Manifest](successSchema: DataDefinitionOp[A]) extends RestOp[A]

  case class Update[I,E,R](schema: DataDefinitionOp[I],
                           //                      processor: Processor[A, F, B, E],
                           successSchema: DataDefinitionOp[R],
                           failureSchema: DataDefinitionOp[E]) extends RestOp[R]

  case class Delete[I,E,R](processDelete: I => Either[E,R],
                           successSchema: DataDefinitionOp[R]) extends RestOp[R]

  case class Load[T:Manifest,E]()

  case class Search()

}

object Sugar {

  import Algebra._

  def endPoint[A:Manifest](url: String): EndPoint[A] = EndPoint[A](url, List.empty)

}
