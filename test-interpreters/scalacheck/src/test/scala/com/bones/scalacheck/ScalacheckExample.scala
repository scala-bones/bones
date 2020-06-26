package com.bones.scalacheck

import org.scalacheck.Arbitrary
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.Checkers
import com.bones.syntax._

class ScalacheckExample extends AnyFunSuite with Checkers {


  // Define a case class.  This is probably already be defined in your application.
  object EyeColor extends Enumeration {
    type EyeColor = Value
    val Amber, Blue, Brown, Grey, Green, Hazel, Red = Value
  }

  case class PersonalTraits(height: Int, weight: Double, eyeColor: EyeColor.EyeColor, correctiveVision: Boolean)

  // Define our "Bones Schema" with data constraints
  val personalTraitsSchema = (
    ("height", int(iv.min(12), iv.max(240))) ::
      ("weight", double(dv.min(4), dv.max(9999)), "Weight in Pounds", 205.5) ::
      ("eyeColor", enumeration[EyeColor.type, EyeColor.EyeColor](EyeColor)) ::
      ("correctiveVision", boolean) ::
      kvpNil
    ).convert[PersonalTraits]

  //Use the Scalacheck interpreter to generate an Arbitrary which will produce data within the range of the schema provided above.
  implicit val arb = Arbitrary(Scalacheck.createCustomGen(personalTraitsSchema, com.bones.scalacheck.custom.allInterpreters))


  //Write your tests
  test("user generator") {
    check( (personalTraits: PersonalTraits) => {

      personalTraits.height >= 12 &&
        personalTraits.height <=240 &&
        personalTraits.weight >= 4 &&
        personalTraits.weight <= 9999 &&
        EyeColor.values.contains(personalTraits.eyeColor) &&
        List(true, false).contains(personalTraits.correctiveVision)

    })
  }







}
