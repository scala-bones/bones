package com.bones.http

import com.bones.data.Error.ExtractionErrors

package object common {
  type ValidatorFunc[A] = Array[Byte] => Either[ExtractionErrors[String], A]
  type EncoderFunc[A] = A => Array[Byte]

}
