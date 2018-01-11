package com.gaia.soy

import java.util.{Date, UUID}

import cats.data.Validated
import cats.data.Validated.{Invalid, Valid}
import com.gaia.soy.StringValidation.{Max, OptionalString, RequiredString}
import com.gaia.soy.compiler.JsonCompiler._
import com.gaia.soy.producer.LiftJson
import org.scalatest.FunSuite

class ExampleTest extends FunSuite {


  abstract class NoneJsonProducer extends JsonProducer {
    override def produceBool(key: Key): Either[WrongTypeError[Boolean], Option[Boolean]] = Right(None)
    override def produceString(key: Key): Either[WrongTypeError[String], Option[String]] = Right(None)
    override def produceBigDecimal(key: Key): Either[WrongTypeError[BigDecimal], Option[BigDecimal]] = Right(None)
    override def produceInt(key: Key): Either[WrongTypeError[Int], Option[Int]] = Right(None)
    override def produceObject(key: Key): Either[WrongTypeError[JsonProducer], Option[JsonProducer]] = Right(None)
  }

//  test("sibling groups") {
//    case class User(username: String, pass: String, message: Option[String], role: Option[String])
//    case class Car(make: String, model: String, year: Option[String])
//
//    val rvp = new NoneJsonProducer {
//      override def produceString(key: Key): Either[WrongTypeError[String], Option[String]] = key.name match {
//        case "username" => Right(Some("Thisisusername"))
//        case "password" => Right(Some("thisispassword"))
//        case "message" => Right(None)
//        case "role" => Right(Some("employee"))
//      }
//    }
//
//    Soyo.obj2(
//      key("user").obj4(
//        key("username").string.alphanum(),
//        key("password").string(),
//        key("message").string().optional(),
//        key("role").string().valid("manager", "employee").optional(),
//        User(_,_,_,_)
//      ),
//      key("car").obj4(
//        key("make").string.alphanum(),
//        key("model").string(),
//        key("message").string().optional(),
//        key("role").string().valid("manager", "employee").optional(),
//        User(_,_,_,_)
//      )
//    )
//
//  }

  test("group") {

    case class Location(countryIso: String, postalCode: Option[String])
    case class User(username: String, pass: String, message: Option[String], location: Option[Location])

    val rvp = new NoneJsonProducer {
      override def produceString(key: Key): Either[WrongTypeError[String], Option[String]] = key match {
        case StringKey(name) => {
          name match {
            case "username" => Right(Some("Thisisusername"))
            case "password" => Right(Some("thisispassword"))
            case "message" => Right(None)
            case "role" => Right(Some("employee"))
            case "countryIso" => Right(Some("US"))
            case "postalCode" => Right(Some("28791"))
          }
        }
        case RootKey => ???
      }

      override def produceObject(key: Key): Either[WrongTypeError[JsonProducer], Option[JsonProducer]] = Right(Some(this))
    }

    //Create the AST
    val prog = obj.obj4(
      key("username").string().alphanum(),
      key("password").string(),
      key("message").string().optional(),
      key("location").obj2(
        key("countryIso").string(),
        key("postalCode").string().optional(),
        Location(_:String,_:Option[String])
      ).optional(),
      User(_:String,_:String,_:Option[String],_:Option[Location]) //Type annotations required for scala 2.11
    ).lift

    import cats.implicits._
    //create the program that is responsible for converting JSON into a User.
    val jsonToUserProgram = prog.foldMap[FromProducer](defaultCompiler)

    //Here we run the program by giving
    val user = jsonToUserProgram.apply(rvp)

    assert( user == Valid( User("Thisisusername", "thisispassword", None, Some(Location("US", Some("28791"))))) )

    val desc = prog.foldMap[Doc](docCompiler)

    println(desc)


  }

  test("alphanum passes") {

    val alpha = key("username").string().alphanum().optional()

    val validInput = new NoneJsonProducer {
      override def produceString(key: Key): Either[WrongTypeError[String], Option[String]] = {
        Right(Some("thisisalphanum"))
//        Right(Coproduct[JSON]("thisisalphanum").select[String])
      }
    }


    val invalidInput = new NoneJsonProducer {
      override def produceString(key: Key): Either[WrongTypeError[String], Option[String]] = Right(Some("Not(&*&Valid"))
    }

    assert( alpha.extract(validInput) == Valid(Some("thisisalphanum")))
    assert( alpha.extract(invalidInput).isInvalid)




  }

  test("passes multiple tests") {
    case class User(username: String, password: String, accessToken: Either[String, Int], birthyear: Int, email: String)

    val x = key("username").string.alphanum.min(3).max(7).optional()

    val validInput = new NoneJsonProducer {
      override def produceString(key: Key): Either[WrongTypeError[String], Option[String]] = Right(Some("valid"))
    }

    val failsOne = new NoneJsonProducer {
      override def produceString(key: Key): Either[WrongTypeError[String], Option[String]] = Right(Some("thisistoolong"))
    }

    val failsTwo = new NoneJsonProducer {
      override def produceString(key: Key): Either[WrongTypeError[String], Option[String]] = Right(Some("$3"))
    }

    assert( x.extract(validInput) == Valid(Some("valid")))


    x.extract(failsOne) match {
      case Invalid(nel) => {
        assert( nel.size == 1)
        nel.head match {
          case ValidationError(k, exOp, i) => assert(exOp.isInstanceOf[Max])
          case x => fail("Expected ValidationError, received: " + x)
        }
      }
      case x => fail("Expected Invalid, received:" + x)
    }

    x.extract(failsTwo) match {
      case Invalid(nel) => {
        assert( nel.size == 2)
      }
      case x => fail("Expected Invalid, received:" + x)
    }
  }


  test("passes bt cc") {

    import conversions._
    sealed abstract class CreditCardType(val abbrev : String)
    object CreditCardTypes extends Enumeration {
      case object Visa extends CreditCardType("Visa")
      case object Mastercard extends CreditCardType("Mastercard")
      case object Amex extends CreditCardType("Amex")
      case object Discover extends CreditCardType("Discover")
    }
    implicit class RequiredStringToCreditCardType(requiredString: RequiredString) {
      def asCreditCardType() : RequiredUuidExtraction = RequiredUuidExtraction(requiredString)
    }
    def toCreditCardType: String => Validated[String,CreditCardType] = input => {
      input.toLowerCase match {
        case "visa" => Valid(CreditCardTypes.Visa)
        case "mastercard" => Valid(CreditCardTypes.Mastercard)
        case "amex" => Valid(CreditCardTypes.Amex)
        case "discover" => Valid(CreditCardTypes.Discover)
        case x => Invalid(s"input: ${x} is not a valid credit card type")
      }
    }




    case class BillingLocation(countryIso: String, zipCode: Option[String])
    case class BtCc(firstFive: String, lastFour: String, uuid: UUID, token: UUID, ccType: CreditCardType,
      expMonth: Int, expYear: Int, cardholder: String, currencyIso: String, deletedAt: Option[Date], lastModifiedRequest: UUID, billingLocation: Option[BillingLocation])
    val isoList = List("US", "CA", "MX")




    val prog = obj.obj12 (
      key("firstFive").string().matchesRegex("[0-9]{5}".r),
      key("lastFour").string().matchesRegex("[0-9]{4}".r),
      key("uuid").string().asUuid(),
      key("token").string().asUuid(),
      key("ccType").string().custom("to CreditCardType", toCreditCardType),
      key("expMonth").int().between(1,12),
      key("expYear").int().between(1950,9999),
      key("cardHolder").string(),
      key("currencyIso").string().length(3),
      key("deletedAt").string().optional().asIsoDateTime(),
      key("lastModifiedRequest").string().asUuid(),
      key("billingLocation").obj2(
        key("countryIso").string().valid(isoList:_*),
        key("zipCode").string().optional(),
        BillingLocation(_: String, _:Option[String])
      ).optional(),
      BtCc(_: String, _: String, _: UUID, _: UUID, _:CreditCardType,_:Int, _:Int, _:String, _: String, _:Option[Date], _: UUID, _:Option[BillingLocation])
    )


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

    val parsed = net.liftweb.json.parse(cc)
    val jsonProducer = LiftJson(parsed)

    import cats.implicits._
    //create the program that is responsible for converting JSON into a User.
//    val jsonToUserProgram = prog.foldMap[FromProducer](defaultCompiler)

    //Here we run the program by giving
//    val btCc = jsonToUserProgram.apply(jsonProducer)
    val btCc = prog.extract(jsonProducer)

    assert( btCc == Valid(BtCc("12345", "4321", UUID.fromString("df15f08c-e6bd-11e7-aeb8-6003089f08b4"),
      UUID.fromString("e58e7dda-e6bd-11e7-b901-6003089f08b4"), CreditCardTypes.Mastercard, 11, 2022,
      "Lennart Augustsson", "USD" , None, UUID.fromString("4545d9da-e6be-11e7-86fb-6003089f08b4"),
      Some(BillingLocation("US", Some("80031")))
    )))

//    val desc = prog.foldMap[Doc](docCompiler)
//
//    println(desc)




  }

}
