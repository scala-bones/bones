package com.ot

import cats.free.FreeApplicative
import com.ot.bones.db.DatabaseSyntax
import com.ot.bones.http.HttpSyntax
import com.ot.bones.transform.TransformSyntax
import com.ot.bones.validation.{CustomConversionFromString, UuidValidation, _}

package object bones {

  /** Bones is the base class defining the FreeAp for each field group defined.*/
  trait ProgramModuleOp[A] {
    //lift any BonesOp into a FreeApplicative
    def lift: ProgramModule[A] = FreeApplicative.lift(this)
  }

  type ProgramModule[A] = FreeApplicative[ProgramModuleOp, A]

  object everything extends UuidValidation with CustomConversionFromString with DateValidation with TransformSyntax
    with KeySyntax with DatabaseSyntax with HttpSyntax with ValidateSyntax

  object obj extends ToHList

}
