package com.bones.interpreter.deltavalidator

import com.bones.Util.NullableResult
import com.bones.data.Error.ExtractionErrors

trait DeltaKvpValidator[K, ALG[_], A, IN] {
  def validate(in: IN, path: List[String]): Either[ExtractionErrors[K], NullableResult[K, A]]
}
