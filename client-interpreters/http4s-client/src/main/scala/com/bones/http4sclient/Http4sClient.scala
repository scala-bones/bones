package com.bones.http4sclient

import java.nio.charset.Charset

import cats.data.NonEmptyList
import cats.effect.IO
import com.bones.circe.CirceValidatorInterpreter
import com.bones.data.Error.ExtractionError
import com.bones.data.KvpCollection
import com.bones.interpreter.InterchangeFormatValidatorValue
import io.circe.Json
import org.http4s.client.Client

object Http4sClient {

  type ID = Long

  type Error[E] = NonEmptyList[Either[ExtractionError, E]]

  def getF[ALG[_], O, E](
    path: String,
    outputSchema: KvpCollection[ALG, O],
    circeValidator: CirceValidatorInterpreter[ALG],
    charset: Charset,
    validatorInterpreter: InterchangeFormatValidatorValue[ALG, Json]
  ): Client[IO] => ID => IO[Either[Error[E], O]] = {

    val f = circeValidator.generateByteArrayValidator(outputSchema, charset)

    httpClient => id =>
      {
        httpClient
          .expect[Array[Byte]](s"http://localhost:8080/$path/$id")
          .map(bytes => f.apply(bytes))
        ???
      }
  }

}
