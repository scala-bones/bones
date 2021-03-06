package com.bones.protobuf

import com.bones.protobuf.messageType.defaultProtoFile
import com.bones.protobuf.values.{defaultEncoder, defaultUtcValidator}
import com.bones.scalacheck.values._
import com.bones.schemas.Schemas
import com.bones.schemas.Schemas.{AllSupported, allSupportCaseClass}
import org.scalacheck.Arbitrary
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.Checkers

class ProtobufScalacheckTest extends AnyFunSuite with Checkers {

  val encode = defaultEncoder
    .generateProtobufEncoder(Schemas.allSupportCaseClass)
  val decode = defaultUtcValidator
    .fromCustomBytes(Schemas.allSupportCaseClass)

  implicit val arb = Arbitrary(
    defaultValuesScalacheck.generateGen(allSupportCaseClass)
  )

  ignore("scalacheck allSupport types - marshall then marshall") {
    check((cc: AllSupported) => {

      val bytes = encode(cc)

//      println(javax.xml.bind.DatatypeConverter.printHexBinary(bytes))

      val newCc = try {
        decode(bytes)
      } catch {
        case ex: Exception =>
          ex.printStackTrace()
          throw ex
      }

      newCc match {
        case Left(x) =>
          fail(s"expected success, received $x for protobuff bytes ${bytes}")
        case Right(newCc2) =>
          val nullBa = Array[Byte]()

          //Arrays seem to only be equal when they reference the same object, so let's remove them form the whole object copy
          val newCc2NoBa = newCc2.copy(ba = nullBa).copy(child = newCc2.child.copy(ba = None))
          val ccNoBA = cc.copy(ba = nullBa).copy(child = cc.child.copy(ba = None))

          if (!(newCc2NoBa == ccNoBA && java.util.Arrays.equals(newCc2.ba, cc.ba))) {
            println(cc)
            println(newCc2)
          }

          newCc2NoBa == ccNoBA && java.util.Arrays.equals(newCc2.ba, cc.ba)
      }
    })

  }

  // Print the file, to be used with the protobufIntegrationTest
  test("print protofile") {
    val message = defaultProtoFile
      .fromSchemaCustomAlgebra(allSupportCaseClass)
    print(defaultProtoFile.messageToProtoFile(message))
  }

}
