package com.bones.httpcommon

import com.bones.syntax.{int, iv, long, lv, uuid}

object DefaultIdDefinitions {
  val longIdDefinition = long(lv.positive)
  val intIdDefinition = int(iv.positive)
  val uuidIdDefinition = uuid()

}
