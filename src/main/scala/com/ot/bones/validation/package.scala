package com.ot.bones

import cats.free.FreeApplicative

package object validation {

  /** DataDefinitionOp is the base class defining the FreeAp for each data definition..*/
  trait DataDefinitionOp[A] {
    //lift any BonesOp into a FreeApplicative
    def lift: DataDefinition[A] = FreeApplicative.lift(this)
  }

  type DataDefinition[A] = FreeApplicative[DataDefinitionOp, A]

}
