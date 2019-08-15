package com

import com.bones.data.{KeyValueDefinitionSugar, Sugar}
import com.bones.validation.ValidationDefinition._

package object bones {

  /** So we can just import com.bones.syntax._ */
  object syntax extends Sugar with KeyValueDefinitionSugar {

    /** sv = String Validation */
    val sv = StringValidation

    /** lv = Long validation */
    val lv = LongValidation

    /** iv = Int validation */
    val iv = IntValidation

    /** bdv = Big Decimal Validation */
    val bdv = BigDecimalValidation

    /** fv = Float Validation */
    val fv = FloatValidation

    /** cv = Char Validation */
    val cv = CharValidation

    /** bv = Byte Validation */
    val bv = ByteValidation

    /** shv = Short Validation */
    val shv = ShortValidation

    /** dv = double validation */
    val dv = DoubleValidation

  }

}
