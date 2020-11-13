package com.bones.httpcommon

import com.bones.validation.ValidationDefinition.ValidationOp

package object search {

  trait SearchParameters[A] {
    def validations: List[ValidationOp[A]]
  }
  case class Numerical[ALG[_], A](name: String, valueSchema: ALG[A], validationOp: ValidationOp[A])

}
