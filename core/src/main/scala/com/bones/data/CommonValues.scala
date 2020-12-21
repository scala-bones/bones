package com.bones.data

import com.bones.data.values.DefaultValues
import com.bones.syntax.{int, iv, long, lv, uuid, uuidV}

import java.util.UUID

object CommonValues {
  val longIdDefinition: DefaultValues[Long] = long(lv.positive, lv.unique)
  val intIdDefinition: DefaultValues[Int] = int(iv.positive, iv.unique)
  val uuidIdDefinition: DefaultValues[UUID] = uuid(uuidV.unique)
}
