package com.ot.bones

import cats.free.FreeApplicative

package object validation {

  /** Bones is the base class defining the FreeAp for each field group defined.*/
  trait DataDefinitionOp[A] {
    //lift any BonesOp into a FreeApplicative
    def lift: DataDefinition[A] = FreeApplicative.lift(this)
  }

  type DataDefinition[A] = FreeApplicative[DataDefinitionOp, A]

}
