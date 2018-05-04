# Overview

The idea behind Bones is to create a clone of Joi validation (written in JavaScript) by building up an 
Abstract Syntax Tree (AST) and then running the tree through different interpreters which will
generate programs to Encode, Decode JSON as well as creating documentation and essentually anything 
that depends on the Schema.

## Download

### Stableish
Core.
```libraryDependencies += "com.github.oletraveler" %% "bones" % "0.3.0"```

Interpreters (no need to include core if you include this)
#### Lift Json Interpreter (currently the only interpreter)
```libaryDependencies += "com.github.oletraveler" %% "bones-rest-unfiltered" % "0.4.0"```

### Snapshot
```libaryDependencies += "com.github.oletraveler" %% "bones" % "0.4.0-SNAPSHOT"```

Interpreters (no need to include core if you include this)
#### Lift Json Interpreter (currently the only interpreter)
```libaryDependencies += "com.github.oletraveler" %% "bones-rest-unfiltered" % "0.4.0-SNAPSHOT"```


## Interpreters

* Marshall from Json to case class (Complete)
* Unmarshall case class to Json (Complete)
* Documentation Interpreter (maybe Swagger or Json API) (POC Complete)
* JavaScript/Html Generator (Not Implemented)
* DB Schema Generator (Not Implemented)

Since using the GADT types directly can become unwieldy, Bones also has a declarative syntax interface in order to 
simplify the creation of the GADTs by using a DSL.

## Getting Started

Here is an example data definition.

```$scala
    val extractData = obj12(
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
        key("countryIso").string(sv.validValue(isoVector)),
        key("zipCode").string().optional()
      ).transform[BillingLocation].optional()
    ).transform[CC]
``` 

And here we can see the full context in the [Example Test](src/test/scala/com/ot/bones/ValidationTest.scala)

```$scala


    //Define some example data types.
    /** CreditCardType */
    sealed abstract class CreditCardType(val abbrev: String)

    object CreditCardTypes {
      case object Visa extends CreditCardType("Visa")
      case object Mastercard extends CreditCardType("Mastercard")
      case object Amex extends CreditCardType("Amex")
      case object Discover extends CreditCardType("Discover")

      def toCreditCardType: String => Validated[ConversionError[String, CreditCardType], CreditCardType] = input => {
        input.toLowerCase match {
          case "visa" => Valid(CreditCardTypes.Visa)
          case "mastercard" => Valid(CreditCardTypes.Mastercard)
          case "amex" => Valid(CreditCardTypes.Amex)
          case "discover" => Valid(CreditCardTypes.Discover)
          case x => Invalid(ConversionError(x, classOf[CreditCardType]))
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
    val extractData = obj12(
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
        key("countryIso").string(sv.validValue(isoVector)),
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
    val jsonToCCProgram = extractData.lift.foldMap[ValidateFromProducer](DefaultExtractInterpreter())

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
    val ccToJson = extractData.lift.foldMap[Encode](DefaultEncoderInterpreter())
    import net.liftweb.json._
    val output = ccToJson.apply(btCc.toOption.get)
    val printed = compact(render(output))
    assert(printed === """{"firstFive":"12345","lastFour":"4321","uuid":"df15f08c-e6bd-11e7-aeb8-6003089f08b4","token":"e58e7dda-e6bd-11e7-b901-6003089f08b4","ccType":"Mastercard","expMonth":11,"expYear":2022,"cardHolder":"Lennart Augustsson","currencyIso":"USD","lastModifiedRequest":"4545d9da-e6be-11e7-86fb-6003089f08b4","billingLocation":{"countryIso":"US","zipCode":"80031"}}""")


    //Doc interpreter, simple POC showing we can make documentation out of this.
    val docOut = extractData.lift.foldMap[Doc](DocInterpreter)
    assert(docOut.str === """Transform to a CC$3 from object with 12 members: [firstFive: String,lastFour: String,uuid: String representing a UUID,token: String representing a UUID,ccType: Convert to a CreditCardType from String,expMonth: Int,expYear: Int,cardHolder: String,currencyIso: String with one of the following values: [CAD,GBP,USD],deletedAt: Optional: Date with format ISO date-time format with the offset and zone if available, such as '2011-12-03T10:15:30', '2011-12-03T10:15:30+01:00' or '2011-12-03T10:15:30+01:00[Europe/Paris]',lastModifiedRequest: String representing a UUID,billingLocation: Optional: Convert to a Transform to type BillingLocation$3 from object with 2 members: [countryIso: String,zipCode: Optional: String]]""")

  }
```



## Credits

* "Your bones got a little machine" - Black Francis
* The API for this project is adapted from the [Joi Project](https://github.com/hapijs/joi) .
* John De Goes [Free Applicative Talk](https://www.youtube.com/watch?v=H28QqxO7Ihc)
* Kris Knuttycombe's [Xenomorph Library](https://github.com/nuttycom/xenomorph) is similar to this.
* Scodec is an amazing library.  I learned a lot from that library.

