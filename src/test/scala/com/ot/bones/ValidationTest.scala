package com.ot.bones

import java.util.{Date, UUID}

import cats.Id
import cats.data.Validated
import cats.data.Validated.{Invalid, Valid}
import com.ot.bones.data.Algebra.{DataDefinitionOp, StringData}
import com.ot.bones.data.Key
import com.ot.bones.interpreter.DocInterpreter.{Doc, DocInterpreter}
import com.ot.bones.interpreter.ExtractionInterpreter.{CanNotConvert, DefaultExtractInterpreter, JsonProducer, ValidateFromProducer, WrongTypeError}
import com.ot.bones.producer.LiftJsonProducer
import com.ot.bones.rest.Algebra.Processor
import com.ot.bones.validation.ValidationDefinition.{IntValidation => iv, StringValidation => sv}
import org.scalatest.FunSuite


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


  test("validation example") {

    //Define some example data types.
    /** CreditCardType */
    sealed abstract class CreditCardType(val abbrev: String)

    object CreditCardTypes {
      case object Visa extends CreditCardType("Visa")
      case object Mastercard extends CreditCardType("Mastercard")
      case object Amex extends CreditCardType("Amex")
      case object Discover extends CreditCardType("Discover")

      def toCreditCardType: String => Validated[CanNotConvert[String, CreditCardType], CreditCardType] = input => {
        input.toLowerCase match {
          case "visa" => Valid(CreditCardTypes.Visa)
          case "mastercard" => Valid(CreditCardTypes.Mastercard)
          case "amex" => Valid(CreditCardTypes.Amex)
          case "discover" => Valid(CreditCardTypes.Discover)
          case x => Invalid(CanNotConvert(x, classOf[CreditCardType]))
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

    import com.ot.bones.syntax._

    // Here we are defining our expected input data.  This definition will drive the interpreters.
    val creditCardSchema = obj12(
      key("firstFive").string(sv.length(5), sv.matchesRegex("[0-9]{5}".r)),
      key("lastFour").string(sv.length(4), sv.matchesRegex("[0-9]{4}".r)),
      key("uuid").uuid(),
      key("token").uuid(),
      key("ccType").string().convert(CreditCardTypes.toCreditCardType, (cct: CreditCardType) => cct.abbrev, "CreditCardType", List.empty),
      key("expMonth").int(iv.between(1, 12)),
      key("expYear").int(iv.between(1950, 9999)),
      key("cardHolder").string(),
      key("currencyIso").enumeration(Currency),
      key("deletedAt").isoDateTime().optional(),
      key("lastModifiedRequest").uuid(),
      key("billingLocation").obj2(
        key("countryIso").string(sv.validVector(isoVector)),
        key("zipCode").string().optional()
      ).transform[BillingLocation].optional()
    ).transform[CC]

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
    import com.ot.bones.interpreter.EncoderInterpreter._
    val ccToJson = creditCardSchema.lift.foldMap[Encode](DefaultEncoderInterpreter())
    import net.liftweb.json._
    val output = ccToJson.apply(btCc.toOption.get)
    val printed = compact(render(output))
    assert(printed === """{"firstFive":"12345","lastFour":"4321","uuid":"df15f08c-e6bd-11e7-aeb8-6003089f08b4","token":"e58e7dda-e6bd-11e7-b901-6003089f08b4","ccType":"Mastercard","expMonth":11,"expYear":2022,"cardHolder":"Lennart Augustsson","currencyIso":"USD","lastModifiedRequest":"4545d9da-e6be-11e7-86fb-6003089f08b4","billingLocation":{"countryIso":"US","zipCode":"80031"}}""")


    //Doc interpreter, simple POC showing we can make documentation out of this.
    val docOut = creditCardSchema.lift.foldMap[Doc](DocInterpreter)
    assert(docOut.str === """Transform to a CC$3 from object with 12 members: [firstFive: String,lastFour: String,uuid: String representing a UUID,token: String representing a UUID,ccType: Convert to a CreditCardType from String,expMonth: Int,expYear: Int,cardHolder: String,currencyIso: String with one of the following values: [CAD,GBP,USD],deletedAt: Optional: Date with format ISO date-time format with the offset and zone if available, such as '2011-12-03T10:15:30', '2011-12-03T10:15:30+01:00' or '2011-12-03T10:15:30+01:00[Europe/Paris]',lastModifiedRequest: String representing a UUID,billingLocation: Optional: Convert to a Transform to type BillingLocation$3 from object with 2 members: [countryIso: String,zipCode: Optional: String]]""")




    import com.ot.bones.rest.Sugar._

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
