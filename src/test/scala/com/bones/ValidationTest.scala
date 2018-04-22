package com.bones

import java.time.LocalDateTime
import java.util.{Date, UUID}

import cats.data.Validated
import cats.data.Validated.Valid
import com.bones.data.Algebra.{DataDefinitionOp, StringData}
import com.bones.data.Key
import com.bones.interpreter.DocInterpreter.{Doc, DocInterpreter}
import com.bones.interpreter.ExtractionInterpreter.{CanNotConvert, DefaultExtractInterpreter, JsonProducer, ValidateFromProducer, WrongTypeError}
import com.bones.producer.LiftJsonProducer
import com.bones.rest.Algebra.Processor
import com.bones.validation.ValidationDefinition.{ValidationOp, IntValidation => iv, StringValidation => sv}
import org.scalatest.FunSuite
import shapeless.HNil



class ValidationTest extends FunSuite {

  abstract class NoneJsonProducer extends JsonProducer {
    override def produceBool: Validated[WrongTypeError[Boolean], Option[Boolean]] = Valid(None)

    override def produceString: Validated[WrongTypeError[String], Option[String]] = Valid(None)

    override def produceDouble: Validated[WrongTypeError[Double], Option[Double]] = Valid(None)

    override def produceInt: Validated[WrongTypeError[Int], Option[Int]] = Valid(None)

    override def produceObject: Validated[WrongTypeError[JsonProducer], Option[JsonProducer]] = Valid(None)

    override def produceList: Validated[WrongTypeError[List[_]], Option[List[JsonProducer]]] = Valid(None)

    override def child(key: Key): JsonProducer = this
  }

  test("append obj") {

    import com.bones.syntax._

    val o1 = obj2(
      key("key1").string(),
      key("key2").string()
    )

    val o2 = obj2(
      key("key3").string(),
      key("key4").string()
    )

//    val o3 = o1 append o2
  }

  test ("issue") {
//    import shapeless._
//
//    case class Wrap[L <: HList](wrapped: L)
//
//    val x = Wrap("x" :: "y" :: HNil)
//    val y = Wrap(1 :: 2 :: HNil)
//
//    case class Append[L1, L2](w1: Wrap[L1], w2: Wrap[L2], prepend: Prepend[L1, L2], length: Length[L1])
//
//    def append[L1, L2](w1: Wrap[L1], w2: Wrap[L2])(implicit prepend: Prepend[L1, L2], length: Length[L1]) = Append(w1, w2, prepend, length)
//
//    val xAppendY = append(x,y)
//
//    val merged = xAppendY.prepend(xAppendY.w1.wrapped, xAppendY.w2.wrapped)
//
//    val split = Split[xAppendY.prepend.Out, xAppendY.hLength.Out]
//
//    split.apply(merged)




  }

  test ("append generalization") {

    import com.bones.syntax._


    val s2 = obj2(
      key("val1").string(),
      key("val2").string()
    )
    val i2 = obj2(
      key("int1").int(),
      key("int2").int()
    )
    val merged = s2 :: i2

//    val s = implicitly[Split[result.p.Out, result.lpLength.Out]]
//    val s = Split[String :: String :: Int :: Int, Nat._2]

    case class Out(s1: String, s2: String, i1: Int, i2: Int)

    val input =
      """
        |{
        |  "val1" : "one",
        |  "val2" : "two",
        |  "int1" : 1,
        |  "int2" : 2
        |}
      """.stripMargin
    val parsed = net.liftweb.json.parse(input)
    val jsonProducer = LiftJsonProducer(parsed)

//    val extResult = merged.apply(jsonProducer)
//
//    assert(extResult.toOption.get === Out("one", "two", 1, 2))


  }

  test("text max prepend") {
//    import com.bones.syntax._
//
//    val x = key("test").string().optional().asMember
//
//    val y = x :: x :: x :: x :: x :: x :: x :: x :: x :: x :: x :: x :: x :: x :: x :: x :: x :: x :: x :: x :: x :: x :: x :: x :: x :: x :: x :: x :: x :: x :: x :: x :: x :: x :: x :: x ::
//      x :: x :: x :: x :: x :: x :: x :: x :: x :: x :: x :: x :: x :: x :: x :: x :: x :: x :: x :: x :: x :: x :: x :: x
//    val cc =
//      """
//        |{
//        |  "test" : "12345",
//        |  "lastFour" : "4321",
//        |  "uuid" : "df15f08c-e6bd-11e7-aeb8-6003089f08b4",
//        |  "token" : "e58e7dda-e6bd-11e7-b901-6003089f08b4",
//        |  "ccType" : "mastercard"
//        |}
//      """.stripMargin
//
//    //sorry, we still use lift in my projects.  I will soon create a Circe JsonProducer.
//    val parsed = net.liftweb.json.parse(cc)
//    val jsonProducer = LiftJsonProducer(parsed)
//
//    //create the program that is responsible for converting JSON into a CC.
//    val jsonToCCProgram = y.lift.foldMap[ValidateFromProducer](DefaultExtractInterpreter())
//
//    //here, we will test that just the validation step is working
//    val btCc = jsonToCCProgram.apply(jsonProducer)
//    btCc.isValid

  }

  test("validation example") {

    //Define some example data types.
    /** CreditCardType */
    sealed abstract class CreditCardType(val abbrev: String)

    object CreditCardTypes {
      case object Visa extends CreditCardType("Visa")
      case object Mastercard extends CreditCardType("Mastercard")
      case object Amex extends CreditCardType("Amex")
      case object Discover extends CreditCardType("Discover")

      def toCreditCardType: String => Either[CanNotConvert[String, CreditCardType], CreditCardType] = input => {
        input.toLowerCase match {
          case "visa" => Right(CreditCardTypes.Visa)
          case "mastercard" => Right(CreditCardTypes.Mastercard)
          case "amex" => Right(CreditCardTypes.Amex)
          case "discover" => Right(CreditCardTypes.Discover)
          case x => Left(CanNotConvert(x, classOf[CreditCardType]))
        }
      }
    }


    case class BillingLocation(countryIso: String, zipCode: Option[String])

    object Currency extends Enumeration {
      val USD = Value("USD")
      val CAD = Value("CAD")
      val GBP = Value("GBP")
    }

    case class CC(firstFive: String, lastFour: String, uuid: UUID, token: UUID, ccType: CreditCardType,
                  expMonth: Int, expYear: Int, cardholder: String, currency: Currency.Value, deletedAt: Option[Date],
                  lastModifiedRequest: UUID, billingLocation: Option[BillingLocation])

    val isoVector = Vector("US", "CA", "MX")



    /** **** Begin Real Example ******/

    import shapeless.::

    object HasNotExpired extends ValidationOp[Int :: Int :: HNil] {
      override def isValid: Int :: Int :: HNil => Boolean = input => {
        val now = LocalDateTime.now()
        val expMonth = input.head
        val expYear = input.tail.head
        if (now.getYear < expYear) true
        else if (now.getYear == expYear && now.getMonthValue >= expMonth) true
        else false

      }

      override def defaultError(t: ::[Int, ::[Int, HNil]]): String = "Expired Card"

      override def description: String = "Credit Card Expiration Date must be in the future"
    }
    import com.bones.syntax._

    val ccExp = obj2(
      key("expMonth").int(iv.between(1,12)),
      key("expYear").int(iv.between(1950, 9999))
    ).validate(HasNotExpired)

    // Here we are defining our expected input data.  This definition will drive the interpreters.
    val obj = obj5(
      key("firstFive").string(sv.length(5), sv.matchesRegex("[0-9]{5}".r)),
      key("lastFour").string(sv.length(4), sv.matchesRegex("[0-9]{4}".r)),
      key("uuid").uuid(),
      key("token").uuid(),
      key("ccType").string().convert(CreditCardTypes.toCreditCardType, (cct: CreditCardType) => cct.abbrev, "CreditCardType", List.empty)
    ) :: ccExp :: obj5(
      key("cardHolder").string(),
      key("currencyIso").enumeration(Currency),
      key("deletedAt").isoDateTime().optional(),
      key("lastModifiedRequest").uuid(),
      key("billingLocation").obj2(
        key("countryIso").string(sv.validVector(isoVector)),
        key("zipCode").string().optional()
      ).transform[BillingLocation].optional()
    )

    val creditCardSchema = obj.transform[CC]

    //final type is basically DataDefinitionOp[CC]

    //Here is our input
    val cc =
      """
        |{
        |  "firstFive" : "12345",
        |  "lastFour" : "4321",
        |  "uuid" : "df15f08c-e6bd-11e7-aeb8-6003089f08b4",
        |  "token" : "e58e7dda-e6bd-11e7-b901-6003089f08b4",
        |  "ccType" : "mastercard",
        |  "expMonth" : 11,
        |  "expYear" : 2022,
        |  "cardHolder" : "Lennart Augustsson",
        |  "currencyIso" : "USD",
        |  "lastModifiedRequest" : "4545d9da-e6be-11e7-86fb-6003089f08b4",
        |  "billingLocation" : {
        |     "countryIso": "US",
        |     "zipCode": "80031"
        |  }
        |}
      """.stripMargin

    //sorry, we still use lift in my projects.  I will soon create a Circe JsonProducer.
    val parsed = net.liftweb.json.parse(cc)
    val jsonProducer = LiftJsonProducer(parsed)

    //create the program that is responsible for converting JSON into a CC.
    val jsonToCCProgram = creditCardSchema.lift.foldMap[ValidateFromProducer](DefaultExtractInterpreter())

    //here, we will test that just the validation step is working
    val btCc = jsonToCCProgram.apply(jsonProducer)

    //tada!  We have can parse input from JsonProducer to CC using our dataDefinition.
    assert(btCc == Valid(CC("12345", "4321", UUID.fromString("df15f08c-e6bd-11e7-aeb8-6003089f08b4"),
      UUID.fromString("e58e7dda-e6bd-11e7-b901-6003089f08b4"), CreditCardTypes.Mastercard, 11, 2022,
      "Lennart Augustsson", Currency.USD, None, UUID.fromString("4545d9da-e6be-11e7-86fb-6003089f08b4"),
      Some(BillingLocation("US", Some("80031")))
    )))

    //convert back to json
    import com.bones.interpreter.EncoderInterpreter._
    val ccToJson = creditCardSchema.lift.foldMap[Encode](DefaultEncoderInterpreter())
    import net.liftweb.json._
    val output = ccToJson.apply(btCc.toOption.get)
    val printed = compact(render(output))
    assert(printed === """{"firstFive":"12345","lastFour":"4321","uuid":"df15f08c-e6bd-11e7-aeb8-6003089f08b4","token":"e58e7dda-e6bd-11e7-b901-6003089f08b4","ccType":"Mastercard","expMonth":11,"expYear":2022,"cardHolder":"Lennart Augustsson","currencyIso":"USD","lastModifiedRequest":"4545d9da-e6be-11e7-86fb-6003089f08b4","billingLocation":{"countryIso":"US","zipCode":"80031"}}""")


    //Doc interpreter, simple POC showing we can make documentation out of this.
    val docOut = creditCardSchema.lift.foldMap[Doc](DocInterpreter)
    println(docOut.str)
    assert(docOut.str === """Transform to a CC$3 from object with 12 members: [firstFive: String,lastFour: String,uuid: String representing a UUID,token: String representing a UUID,ccType: Convert to a CreditCardType from String,expMonth: Int,expYear: Int,cardHolder: String,currencyIso: String with one of the following values: [CAD,GBP,USD],deletedAt: Optional: Date with format ISO date-time format with the offset and zone if available, such as '2011-12-03T10:15:30', '2011-12-03T10:15:30+01:00' or '2011-12-03T10:15:30+01:00[Europe/Paris]',lastModifiedRequest: String representing a UUID,billingLocation: Optional: Convert to a Transform to type BillingLocation$3 from object with 2 members: [countryIso: String,zipCode: Optional: String]]""")




    import com.bones.rest.Sugar._

    object postToProcessor extends Processor[CC, String, CC] {}




//    {
//      "statusCode": 400,
//      "error": "Bad Request",
//      "message": "invalid query"
//    }

    val errorDef: DataDefinitionOp[String] = StringData()

    //Rest test
    endPoint("/creditCard")
        .post(creditCardSchema, postToProcessor, creditCardSchema, errorDef)
        .get(creditCardSchema)
//        .put("/:uuid", creditCardSchema, postToProcessor, successShape, errorShape)
//        .delete("/:uuid", doDelete, successShape, errorShape)

  }

}
