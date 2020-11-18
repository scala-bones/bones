package com.bones.http.common

import java.util.UUID

import com.bones.data.values.DefaultValues
import com.bones.syntax.{int, iv, long, lv, uuid}

object DefaultIdDefinitions {
  val longIdDefinition: DefaultValues[Long] = long(lv.positive)
  val intIdDefinition: DefaultValues[Int] = int(iv.positive)
  val uuidIdDefinition: DefaultValues[UUID] = uuid()
}
