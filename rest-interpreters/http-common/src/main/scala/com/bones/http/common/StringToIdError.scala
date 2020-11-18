package com.bones.http.common

import com.bones.syntax.{kvpNil, string}

object StringToIdError {
  private val stringToIdErrorHList =
    ("input", string()) ::
      ("errorMessage", string()) ::
      kvpNil

  val stringToIdErrorSchema =
    stringToIdErrorHList.convert[StringToIdError]
}

case class StringToIdError(input: String, errorMessage: String)
