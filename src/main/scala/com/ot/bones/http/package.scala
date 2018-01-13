package com.ot.bones

import com.ot.bones.compiler.ExtractionCompiler.JsonProducer

package object http {

  trait HttpDaemon[A]


  case class GetRequest[A](endpoint: String) extends HttpDaemon[A]

  /**
    *
    * @param endpoint URL endpoint, example /users
    * @param f Function that takes a Deserializer and returns an A
    * @tparam A The concrete type returned, for instance 'case class User(x: String)'
    * @tparam D The deserializer, can extract
    */
  case class PostRequest[A,D](endpoint: String, f: (D => A)) extends HttpDaemon[A]
  case class PostResponse[A](serialize: A => JsonProducer)

}
