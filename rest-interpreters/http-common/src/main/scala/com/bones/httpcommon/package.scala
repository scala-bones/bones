package com.bones

import java.nio.charset.Charset

import com.bones.data.Error.ExtractionErrors

package object httpcommon {
  type ValidatorFunc[A] = (Array[Byte], Charset) => Either[ExtractionErrors[String], A]
  type EncoderFunc[A] = A => Array[Byte]

}
