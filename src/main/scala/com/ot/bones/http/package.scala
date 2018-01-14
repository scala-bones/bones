package com.ot.bones

import cats.free.FreeApplicative
import com.ot.bones.validation.DataDefinitionOp

package object http {

  case class GetRequest[A](endpoint: String) extends ProgramModuleOp[Unit => A]

  /**
    *
    * @param endpointPath URL endpoint, example /users
    * @tparam A The concrete type returned, for instance 'case class User(x: String)'
    */
  case class HttpPostEndpoint[A](endpointPath: String) extends ProgramModuleOp[() => A]

  case class HttpResponse[T](dataDefinitionOp: DataDefinitionOp[T]) extends ProgramModuleOp[T => Unit]

  case class HttpErrorResponse[T]() extends ProgramModuleOp[T => Unit]


  trait HttpSyntax {

    def httpPostEndpoint[A](endpointPath: String) = HttpPostEndpoint[A](endpointPath)

    def httpResponse[T](dataDefinitionOp: DataDefinitionOp[T]): HttpResponse[T] = HttpResponse(dataDefinitionOp)

    def httpErrorResponse[T](): HttpErrorResponse[T] = HttpErrorResponse()


  }

}
