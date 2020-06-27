package com.bones.validation.values

import com.bones.data.values.DefaultValues
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.Checkers
import java.time.Instant

class MultiAlgebraTest extends AnyFunSuite with Checkers {

  import com.bones.syntax._

  val multiAlgebraBase =
    ("email", email()) ::
    ("instant", instant(jt_i.min(Instant.parse("2007-12-03T10:15:30.00Z")))) ::
    ("description", string) ::
    kvpNil

  case class MultiAlgebra(email: String, instant: Instant, description: String)

  val multiAlgebraSchema = multiAlgebraBase.convert[MultiAlgebra]








}
