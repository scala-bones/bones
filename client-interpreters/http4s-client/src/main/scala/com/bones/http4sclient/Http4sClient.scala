package com.bones.http4sclient

import java.nio.charset.Charset

import cats.data.NonEmptyList
import cats.effect.IO
import com.bones.circe.{CirceValidatorFromByteArray, CirceValidatorInterpreter}
import com.bones.data.Error.ExtractionError
import com.bones.data.KvpCollection
import com.bones.interpreter.validator.InterchangeFormatValidatorValue
import io.circe.Json
import org.http4s.client.Client

object Http4sClient {

  type ID = Long

  type Error[E] = NonEmptyList[Either[ExtractionError[String], E]]

  def getF[ALG[_], O, E](
    path: String,
    outputSchema: KvpCollection[String, ALG, O],
    circeValidator: CirceValidatorInterpreter[ALG],
    charset: Charset,
    validatorInterpreter: InterchangeFormatValidatorValue[ALG, Json]
  ): Client[IO] => ID => IO[Either[Error[E], O]] = {

    val validator =
      CirceValidatorFromByteArray(charset).andThen(circeValidator.generateValidator(outputSchema))

    httpClient =>
      id => {
        httpClient
          .expect[Array[Byte]](s"http://localhost:8080/$path/$id")
          .map(bytes => validator.validate(bytes))
        ???
      }
  }

}
