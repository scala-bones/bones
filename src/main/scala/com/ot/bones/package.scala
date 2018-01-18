package com.ot

import cats.free.FreeApplicative
import com.ot.bones.transform.TransformSyntax
import com.ot.bones.validation.{CustomConversionFromString, UuidValidation, _}

package object bones {

  object everything extends UuidValidation with CustomConversionFromString with DateValidation with TransformSyntax
    with KeySyntax

  object obj extends ToHList

}
