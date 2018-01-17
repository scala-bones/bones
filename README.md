# Overview

The idea behind Bones is to create a clone of Joi validation (originally written in JavaScript) using 
the concept of the Generalized Abstract Data Type (GADT) that also double as a Free Applicative.  
Using the Cats Free Applicative, we can create different
interpretation of the GADTs. 

Since using the GADT types directly can become unwieldy, Bones also has a declarative syntax interface in order to 
simplify the creation of the GADTs by using a DSL.

## Getting Started

See the [Example Test](src/test/scala/com/ot/bones/ValidationTest.scala)

```$scala
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

    val isoList = List("US", "CA", "MX")


    /** **** Begin Real Example ******/

    import com.ot.bones.everything._


    // Here we are defining our expected input data.  This definition will drive the interpreters.
    val extractData = obj.obj12(
      key("firstFive").string().matchesRegex("[0-9]{5}".r),
      key("lastFour").string().matchesRegex("[0-9]{4}".r),
      key("uuid").string().asUuid(),
      key("token").string().asUuid(),
      key("ccType").string().custom("to CreditCardType", toCreditCardType),
      key("expMonth").int().between(1, 12),
      key("expYear").int().between(1950, 9999),
      key("cardHolder").string(),
      key("currencyIso").string().length(3),
      key("deletedAt").string().optional().asIsoDateTime(),
      key("lastModifiedRequest").string().asUuid(),
      key("billingLocation").obj2(
        key("countryIso").string().valid(isoList: _*),
        key("zipCode").string().optional()
      ).optional().transform[BillingLocation]
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
    val jsonProducer = LiftJson(parsed)

    //create the program that is responsible for converting JSON into a CC.
    val jsonToCCProgram = extractData.lift.foldMap[ValidateFromProducer](DefaultExtractInterpreter())

    //here, we will test that just the validation step is working
    val btCc = jsonToCCProgram.apply(jsonProducer)

    //tada!  We have can parse input from JsonProducer to CC using our dataDefinition.
    assert(btCc == Valid(CC("12345", "4321", UUID.fromString("df15f08c-e6bd-11e7-aeb8-6003089f08b4"),
      UUID.fromString("e58e7dda-e6bd-11e7-b901-6003089f08b4"), CreditCardTypes.Mastercard, 11, 2022,
      "Lennart Augustsson", "USD", None, UUID.fromString("4545d9da-e6be-11e7-86fb-6003089f08b4"),
      Some(BillingLocation("US", Some("80031")))
    )))


    //some documentation
    val doc = extractData.lift.foldMap(DocInterpreter.docInterpreter)
    println(doc.str)
    //Doc(object with 12 members: (Doc(Required String with key firstFive))(Doc(Required String with key lastFour))
    //  (Doc(Converted to UUID)}))(Doc(Converted to UUID)}))(Doc(Custom Conversion: to CreditCardType))
    //  (Doc(Required Int with key expMonth))(Doc(Required Int with key expYear))(Doc(Required String with key cardHolder))
    // (Doc(Required String with key currencyIso))(Doc(Required Date with format java.time.format.DateTimeFormatter$ClassicFormat@30190c55)}))
    // (Doc(Converted to UUID)}))(Doc(converted to Class BillingLocation$3))) mapped into class CC$3
```



## Credits

* "Your bones got a little machine" - Black Francis
* This API for this project is adapted from the [Joi Project](https://github.com/hapijs/joi JOI).
* John De Goes [Free Applicative Talk](https://www.youtube.com/watch?v=H28QqxO7Ihc)
* Kris Knuttycombe's [Xenomorph Library](https://github.com/nuttycom/xenomorph) is similar to this.
* Scodec is an amazing library.  I learned a lot from that library.

