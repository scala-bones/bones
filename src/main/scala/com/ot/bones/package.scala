package com.ot

import com.ot.bones.convert.{DateConversionInstances, UuidConversionInstances}
import com.ot.bones.validation.{KeySyntax, ToHList}

package object bones {

  object everything extends KeySyntax with ToHList with UuidConversionInstances with DateConversionInstances



}
