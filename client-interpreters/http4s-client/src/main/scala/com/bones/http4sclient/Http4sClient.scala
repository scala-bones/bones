package com.bones.http4sclient

import java.nio.charset.Charset

import cats.data.NonEmptyList
import cats.effect.IO
import com.bones.circe.{EncodeToCirceInterpreter, ValidatedFromCirceInterpreter}
import com.bones.crud.Algebra.Read
import com.bones.data.Error.ExtractionError
import org.http4s.client.Client
import org.http4s.client.blaze.BlazeClientBuilder

object Http4sClient {

  type ID = Long

  type Error[E] = NonEmptyList[Either[ExtractionError, E]]



  def getF[O,E](path: String, read: Read[O,E]) : Client[IO] => ID => IO[Either[Error[E],O]] = {
    val f = ValidatedFromCirceInterpreter.isoInterpreter.byteArrayFuncFromSchema(read.outputSchema, Charset.forName("utf-8"))

    httpClient => id => {
//      httpClient.fetch[Array[Byte]](s"http://localhost:8080/$path/$id")
      httpClient.expect[Array[Byte]](s"http://localhost:8080/$path/$id")
        .map(bytes => f.apply(bytes))
      ???
    }
  }

}