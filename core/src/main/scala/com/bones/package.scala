package com

import com.bones.data.custom.{AllCustomAlgebras, AllCustomSyntax}
import com.bones.data.{KvpValue, KeyValueDefinitionSugar, ListData, OptionalKvpValueDefinition, Sugar}
import com.bones.validation.ValidationDefinition._

/**
  * Collect all functionality here so one only needs to specify one import statement: 'com.bones.syntax._'
  */
package object bones {

  type Path = List[String]

  /** So we can just import com.bones.syntax._ */
  object syntax extends Sugar[AllCustomAlgebras] with AllCustomSyntax

}
