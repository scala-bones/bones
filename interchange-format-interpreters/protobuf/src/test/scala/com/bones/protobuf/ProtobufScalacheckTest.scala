package com.bones.protobuf

import com.bones.scalacheck.Scalacheck
import com.bones.schemas.Schemas
import com.bones.schemas.Schemas.CC
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.FunSuite
import org.scalatestplus.scalacheck.Checkers


class ProtobufScalacheckTest extends FunSuite with Checkers {

  implicit val gen: Gen[CC] = Scalacheck.valueDefinition(Schemas.creditCardSchema)
  implicit val arb = Arbitrary(gen)

  val encode = ProtobufSequentialOutputInterpreter.encodeToBytes(Schemas.creditCardSchema)
  val decode = ProtobufSequentialInputInterpreter.fromBytes(Schemas.creditCardSchema)

  ignore  ("marshall and unmarshall") {
    check( (cc: CC) => {
      val bytes = encode(cc)
      val decodedCc = decode(bytes)
      cc === decodedCc
    })
  }


}
