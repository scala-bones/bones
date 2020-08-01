package com.bones.data.values

/**
 * Some interpreters do not support specific structures, use this to report that to the end user.
 */
case class InvalidStructureError(message: String, path:List[String] = List.empty)
  extends Throwable(message)
