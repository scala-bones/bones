package com.bones

package object slick {
  case class SlickColumns(methodName: String, typeName: String, columnName: String)
  case class SlickTable[T](tupleType: List[String], columns: List[SlickColumns])

}
