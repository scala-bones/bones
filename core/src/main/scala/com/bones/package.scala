package com

import com.bones.data.Sugar
import com.bones.data.values.{DefaultValues, DefaultValuesSyntax}

/**
  * Collect all functionality here so one only needs to specify one import statement: 'com.bones.syntax._'
  */
package object bones {

  type Path = List[String]

  /** So we can just import com.bones.syntax._ */
  object syntax extends Sugar[DefaultValues] with DefaultValuesSyntax

  /** KvpValue is meant to be the 'value' of a key value pair.  These are types
   * at the end of the tree -- the leaf values per-say.  They should not recursive.
   * @tparam A The type of the Wrapped Value
   */
  trait KvpValue[A] {
    val manifestOfA: Manifest[A]
  }

}
