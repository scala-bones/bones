package com

import com.bones.data.Sugar
import com.bones.data.values.{DefaultValues, DefaultValuesSyntax}

/** Collect all functionality here so one only needs to specify one import statement:
  * 'com.bones.syntax._'
  */
package object bones {

  type Path[A] = List[A]

  /** So we can just import com.bones.syntax._ */
  object syntax extends Sugar[String, DefaultValues] with DefaultValuesSyntax

  /** PrimitiveValue is meant to be the 'value' of a key value pair. These types are the leaf values
    * per-say. They should not recursive.
    * @tparam A
    *   The type of the Wrapped Value
    */
  trait PrimitiveValue[A] {
    val typeName: String
  }

  abstract class PrimitiveValueManifestTypeName[A: Manifest] extends PrimitiveValue[A] {
    override val typeName: String =
      manifest[A].runtimeClass.getName.split('$').lastOption.getOrElse("")
  }

}
