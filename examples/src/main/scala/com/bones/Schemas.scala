package com.bones

import java.time.{LocalDateTime, ZonedDateTime}
import java.util.UUID

import com.bones.data.Error.CanNotConvert
import com.bones.validation.ValidationDefinition.ValidationOp
import shapeless.HNil
import com.bones.validation.ValidationDefinition.{ValidationOp, IntValidation => iv, StringValidation => sv}


object Schemas {

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
                expMonth: Int, expYear: Int, cardholder: String, jce: JavaCurrencyEnum, currency: Currency.Value, deletedAt: Option[ZonedDateTime],
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
  ) :: ccExp :: obj6(
    key("cardHolder").string(),
    key("currencyEnum").enum(JavaCurrencyEnum.values.toList),
    key("currencyIso").enumeration(Currency),
    key("deletedAt").isoDateTime().optional(),
    key("lastModifiedRequest").uuid(),
    key("billingLocation").obj2(
      key("countryIso").string(sv.validVector(isoVector)),
      key("zipCode").string().optional()
    ).transform[BillingLocation].optional()
  )

  val creditCardSchema = obj.transform[CC]
//  val creditCardWithUuid = obj(key("uuid").uuid()) :: creditCardSchema

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
      |  "currencyEnum" : "GBP",
      |  "currencyIso" : "USD",
      |  "lastModifiedRequest" : "4545d9da-e6be-11e7-86fb-6003089f08b4",
      |  "billingLocation" : {
      |     "countryIso": "US",
      |     "zipCode": "80031"
      |  }
      |}
    """.stripMargin

}
