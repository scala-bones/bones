package com.ot.bones

import java.util.{Date, UUID}

import cats.data.Validated
import cats.data.Validated.{Invalid, Valid}
import com.ot.bones.everything.key
import com.ot.bones.interpreter.DocInterpreter
import com.ot.bones.interpreter.EncoderInterpreter.{DefaultEncoderInterpreter, ValidateAndEncode}
import com.ot.bones.interpreter.ExtractionInterpreter.{DefaultExtractInterpreter, JsonProducer, ValidateFromProducer, ValidationError, WrongTypeError}
import com.ot.bones.producer.LiftJsonProducer
import com.ot.bones.validation.Key
import org.scalatest.FunSuite
import cats.implicits._
import com.ot.bones.validation.ValidationDefinition.StringValidation.Max
import com.ot.bones.validation.ValidationDefinition.{IntValidation => iv, StringValidation => sv}


class ValidationTest extends FunSuite {

  abstract class NoneJsonProducer extends JsonProducer {
    override def produceBool: Validated[WrongTypeError[Boolean], Option[Boolean]] = Valid(None)

    override def produceString: Validated[WrongTypeError[String], Option[String]] = Valid(None)

    override def produceDouble: Validated[WrongTypeError[Double], Option[Double]] = Valid(None)

    override def produceInt: Validated[WrongTypeError[Int], Option[Int]] = Valid(None)

    override def produceObject: Validated[WrongTypeError[JsonProducer], Option[JsonProducer]] = Valid(None)

    override def produceList: Validated[WrongTypeError[List[_]], Option[List[JsonProducer]]] = Valid(None)

    override def resolve(key: Key): JsonProducer = this
  }


  test("validation example") {

    //Define some example data types.
    /** Enumerated CreditCardType */
    sealed abstract class CreditCardType(val abbrev: String)

    object CreditCardTypes extends Enumeration {

      case object Visa extends CreditCardType("Visa")

      case object Mastercard extends CreditCardType("Mastercard")

      case object Amex extends CreditCardType("Amex")

      case object Discover extends CreditCardType("Discover")

    }

    def toCreditCardType: String => Validated[String, CreditCardType] = input => {
      input.toLowerCase match {
        case "visa" => Valid(CreditCardTypes.Visa)
        case "mastercard" => Valid(CreditCardTypes.Mastercard)
        case "amex" => Valid(CreditCardTypes.Amex)
        case "discover" => Valid(CreditCardTypes.Discover)
        case x => Invalid(s"input: ${x} is not a valid credit card type")
      }
    }
    case class BillingLocation(countryIso: String, zipCode: Option[String])

    case class CC(firstFive: String, lastFour: String, uuid: UUID, token: UUID, ccType: CreditCardType,
                  expMonth: Int, expYear: Int, cardholder: String, currencyIso: String, deletedAt: Option[Date],
                  lastModifiedRequest: UUID, billingLocation: Option[BillingLocation])

    val isoVector = Vector("US", "CA", "MX")


    /** **** Begin Real Example ******/

    import com.ot.bones.everything._
    import shapeless._

    // Here we are defining our expected input data.  This definition will drive the interpreters.
    val extractData = obj9(
      key("firstFive").string(sv.length(5), sv.matchesRegex("[0-9]{5}".r)),
      key("lastFour").string(sv.length(4), sv.matchesRegex("[0-9]{4}".r)),
      key("uuid").string(),
      key("token").string(),
//      key("ccType").string().custom("to CreditCardType", toCreditCardType),
      key("expMonth").int(iv.between(1, 12)),
      key("expYear").int(iv.between(1950, 9999)),
      key("cardHolder").string(),
      key("currencyIso").string(sv.length(3)),
//      key("deletedAt").string().optional().isoDateTime(),
//      key("lastModifiedRequest").string().asUuid(),
      key("billingLocation").obj2(
        key("countryIso").string(sv.validValue(isoVector)),
        key("zipCode").string()
      )
    )
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
    val jsonToCCProgram = extractData.lift.foldMap[ValidateFromProducer](DefaultExtractInterpreter())

    //here, we will test that just the validation step is working
    val btCc = jsonToCCProgram.apply(jsonProducer)

    assert(btCc == Valid(HList("12345", "4321", "df15f08c-e6bd-11e7-aeb8-6003089f08b4",
      "e58e7dda-e6bd-11e7-b901-6003089f08b4", 11, 2022,
      "Lennart Augustsson", "USD", HList("US", "80031")
    )))


    //tada!  We have can parse input from JsonProducer to CC using our dataDefinition.
//    assert(btCc == Valid(CC("12345", "4321", UUID.fromString("df15f08c-e6bd-11e7-aeb8-6003089f08b4"),
//      UUID.fromString("e58e7dda-e6bd-11e7-b901-6003089f08b4"), CreditCardTypes.Mastercard, 11, 2022,
//      "Lennart Augustsson", "USD", None, UUID.fromString("4545d9da-e6be-11e7-86fb-6003089f08b4"),
//      Some(BillingLocation("US", Some("80031")))
//    )))

    //convert back to json
    import com.ot.bones.interpreter.EncoderInterpreter._
    val ccToJson = extractData.lift.foldMap[ValidateAndEncode](DefaultEncoderInterpreter())
    import net.liftweb.json._
    ccToJson.apply(btCc.toOption.get) match {
      case Valid(json) => println(s"JSON: ${pretty(render(json))}")
      case Invalid(err) => fail("expected value, was $err")
    }


    //some documentation
//    val doc = extractData.lift.foldMap(DocInterpreter.DocInterpreter)
//    println(doc.str)
    //Doc(object with 12 members: (Doc(Required String with key firstFive))(Doc(Required String with key lastFour))
    //  (Doc(Converted to UUID)}))(Doc(Converted to UUID)}))(Doc(Custom Conversion: to CreditCardType))
    //  (Doc(Required Int with key expMonth))(Doc(Required Int with key expYear))(Doc(Required String with key cardHolder))
    // (Doc(Required String with key currencyIso))(Doc(Required Date with format java.time.format.DateTimeFormatter$ClassicFormat@30190c55)}))
    // (Doc(Converted to UUID)}))(Doc(converted to Class BillingLocation$3))) mapped into class CC$3

  }




  test("alphanum passes") {


    val alpha = key("username").string(sv.alphanum)

    val validInput = new NoneJsonProducer {
      override def produceString: Validated[WrongTypeError[String], Option[String]] = {
        Valid(Some("thisisalphanum"))
        //        Valid(Coproduct[JSON]("thisisalphanum").select[String])
      }
    }


    val invalidInput = new NoneJsonProducer {
      override def produceString: Validated[WrongTypeError[String], Option[String]] = Valid(Some("Not(&*&Valid"))
    }

    assert( alpha._2.extract(validInput) == Valid(Some("thisisalphanum")))
    assert( alpha._2.extract(invalidInput).isInvalid)




  }

  test("passes multiple tests") {
    case class User(username: String, password: String, accessToken: Validated[String, Int], birthyear: Int, email: String)

    val x = key("username").string(sv.alphanum, sv.min(3), sv.max(7)) //.optional()

    val validInput = new NoneJsonProducer {
      override def produceString: Validated[WrongTypeError[String], Option[String]] = Valid(Some("valid"))
    }

    val failsOne = new NoneJsonProducer {
      override def produceString: Validated[WrongTypeError[String], Option[String]] = Valid(Some("thisistoolong"))
    }

    val failsTwo = new NoneJsonProducer {
      override def produceString: Validated[WrongTypeError[String], Option[String]] = Valid(Some("$3"))
    }

    assert( x._2.extract(validInput) == Valid(Some("valid")))


    x._2.extract(failsOne) match {
      case Invalid(nel) => {
        assert( nel.size == 1)
        nel.head match {
          case ValidationError(exOp, i) => assert(exOp.isInstanceOf[Max])
          case x => fail("Expected ValidationError, received: " + x)
        }
      }
      case x => fail("Expected Invalid, received:" + x)
    }

    x._2.extract(failsTwo) match {
      case Invalid(nel) => {
        assert( nel.size == 2)
      }
      case x => fail("Expected Invalid, received:" + x)
    }
  }


}
