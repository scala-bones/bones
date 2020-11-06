package com.bones.http4s

import cats.data.NonEmptyList
import com.bones.data.Error.{ExtractionError, ExtractionErrors}
import fs2.Stream

object CrudInterpreterDescription {

  /** Collection of items used to to create a Post endpoint in this HttpInterpreter. */
  case class PutPostInterpreterGroup[UI, UE, UO](
    contentType: String,
    inInterpreter: Array[Byte] => Either[ExtractionErrors[String], UI],
    outInterpreter: UO => Array[Byte],
    errorInterpreter: UE => Array[Byte]
  )

  /** Collection of items used to to get a Post endpoint in this HttpInterpreter. */
  case class GetInterpreterGroup[RE, RO](
    contentType: String,
    outInterpreter: RO => Array[Byte],
    errorInterpreter: RE => Array[Byte]
  )

  /** Collection of items used to to create a Delete endpoint in this HttpInterpreter. */
  case class DeleteInterpreterGroup[DE, DO](
    contentType: String,
    outInterpreter: DO => Array[Byte],
    errorInterpreter: DE => Array[Byte]
  )

  /** Collection of items used to to create a Search endpoint in this HttpInterpreter. */
  case class SearchInterpreterGroup[F[_], SO](
    contentType: String,
    outInterpreter: Stream[F, SO] => Stream[F, Array[Byte]]
  )
}
