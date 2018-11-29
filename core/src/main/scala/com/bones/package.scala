package com

import com.bones.data.{KeyValueDefinitionSugar, Sugar}
import com.bones.validation.ValidationDefinition.{BigDecimalValidation, LongValidation, StringValidation}

package object bones {

  object syntax extends Sugar with KeyValueDefinitionSugar {
    val sv = StringValidation
    val lv = LongValidation
    val bdv = BigDecimalValidation
  }




}
