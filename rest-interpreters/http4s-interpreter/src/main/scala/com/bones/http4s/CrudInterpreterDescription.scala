package com.bones.http4s

import cats.data.NonEmptyList
import com.bones.data.Error.ExtractionError
import fs2.Stream

object CrudInterpreterDescription {

  /** Collection of items used to to create a Post endpoint in this HttpInterpreter. */
  case class PutPostInterpreterGroup[UI, UO, UE](
      contentType: String,
      inInterpreter: Array[Byte] => Either[NonEmptyList[ExtractionError], UI],
      outInterpreter: UO => Array[Byte],
      errorInterpreter: UE => Array[Byte]
  )

  /** Collection of items used to to get a Post endpoint in this HttpInterpreter. */
  case class GetInterpreterGroup[RO, RE](
      contentType: String,
      outInterpreter: RO => Array[Byte],
      errorInterpreter: RE => Array[Byte]
  )

  /** Collection of items used to to create a Delete endpoint in this HttpInterpreter. */
  case class DeleteInterpreterGroup[DO, DE](
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
