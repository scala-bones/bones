package com.bones.http.common

import java.util.UUID

import com.bones.Util
import com.bones.syntax.{kvpNil, string}

import scala.util.Try

object Path {

  val intParam: String => Either[StringToIdError, Int] = param => {
    Try {
      param.toInt
    }.toOption
      .toRight(StringToIdError(param, s"Could not convert parameter '${param}' to Int"))
  }

  val longParam: String => Either[StringToIdError, Long] = param => {
    Util
      .stringToLong(param)
      .toRight(StringToIdError(param, s"Could not convert parameter '${param}' to Long"))
  }

  val uuidParam: String => Either[StringToIdError, UUID] = param => {
    Try {
      UUID.fromString(param)
    }.toOption
      .toRight(StringToIdError(param, s"Could not convert parameter '${param}' to UUID"))
  }

}
