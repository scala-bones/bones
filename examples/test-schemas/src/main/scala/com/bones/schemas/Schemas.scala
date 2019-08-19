package com.bones.schemas

import java.time.{LocalDate, LocalDateTime}
import java.util.UUID

import com.bones.data.Error.CanNotConvert
import com.bones.data.Value.KvpNil
import com.bones.validation.ValidationDefinition.ValidationOp
import shapeless.{HNil, Poly1}
import com.bones.syntax._


object Schemas {

  /** This is a product type example */
  sealed abstract class CreditCardType(val abbrev: String)

  object CreditCardTypes {
    case object Visa extends CreditCardType("Visa")
    case object Mastercard extends CreditCardType("Mastercard")
    case object Amex extends CreditCardType("Amex")
    case object Discover extends CreditCardType("Discover")

    def keys = List(Visa, Mastercard, Amex, Discover).map(_.toString)

    def toCreditCardType(input: String, path: List[String]):  Either[CanNotConvert[String, CreditCardType], CreditCardType] = {
      input.toLowerCase match {
        case "visa" => Right(CreditCardTypes.Visa)
        case "mastercard" => Right(CreditCardTypes.Mastercard)
        case "amex" => Right(CreditCardTypes.Amex)
        case "discover" => Right(CreditCardTypes.Discover)
        case x => Left(CanNotConvert(path, x, classOf[CreditCardType]))
      }
    }
  }


  case class BillingLocation(countryIso: String, zipCode: Option[String])

  // Scala Enumeration Example
  object Currency extends Enumeration {
    val USD = Value("USD")
    val CAD = Value("CAD")
    val GBP = Value("GBP")
  }



  case class CC(firstSix: String, lastFour: String, uuid: UUID, token: UUID, ccType: CreditCardType,
                expMonth: Long, expYear: Long, cardholder: String, currency: Currency.Value, deletedAt: Option[LocalDateTime],
                lastModifiedRequest: UUID, billingLocation: Option[BillingLocation])

  val isoList = Vector("US", "CA", "MX")



  /** **** Begin Real Example ******/

  import shapeless.::

  object HasNotExpired extends ValidationOp[Long :: Long :: HNil] {
    override def isValid: Long :: Long :: HNil => Boolean = input => {
      val now = LocalDateTime.now()
      val expMonth = input.head
      val expYear = input.tail.head
      if (now.getYear < expYear) true
      else if (now.getYear == expYear && now.getMonthValue >= expMonth) true
      else false

    }

    override def defaultError(t: ::[Long, ::[Long, HNil]]): String = "Expired Card"

    override def description: String = "Credit Card Expiration Date must be in the future"
  }

  val ccExp = (
    kvp("expMonth", long(lv.between(1,12))) ::
    kvp("expYear", long(lv.between(1950, 9999))) ::
    KvpNil
  ).validate(HasNotExpired)

  val ccTypeValue =
    string().asSumType[CreditCardType](
      "CreditCardType",
        CreditCardTypes.toCreditCardType(_,_),
        (cct: CreditCardType) => cct.abbrev,
        CreditCardTypes.keys,
        List.empty
      )

  // Here we are defining our expected input data.  This definition will drive the interpreters.
  val ccObj = (
    kvp("firstSix", string(sv.length(6), sv.matchesRegex("[0-9]{6}".r))) ::
    kvp("lastFour", string(sv.length(4), sv.matchesRegex("[0-9]{4}".r))) ::
    kvp("uuid", uuid) ::
    kvp("token", uuid) ::
    kvp("ccType", ccTypeValue) ::
    KvpNil
  ) ::: ccExp ::: (
    kvp("cardHolder", string(sv.words)) ::
      kvp("currencyIso", enumeration[Currency.type, Currency.Value](Currency)) ::
      kvp("deletedAt", localDateTime.optional) ::
      kvp("lastModifiedRequest", uuid) ::
      kvp("billingLocation", (
        kvp("countryIso", string(sv.validVector(isoList))) ::
        kvp("zipCode", string(sv.max(10)).optional) ::
        KvpNil
      ).convert[BillingLocation].optional) :: //TODO: Optional
    KvpNil
  )

  case class OasMetadata(example: Option[String], description: Option[String])

  val creditCardSchema = ccObj.convert[CC]

  //final type is basically KvpValue[CC]

  //Here is our input
  val cc =
    """
      |{
      |  "firstSix" : "123456",
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

  val ccBadBilling =
    """
      |{
      |  "firstSix" : "123456",
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
      |     "countryIso": "BZ",
      |     "zipCode": "80031"
      |  }
      |}
    """.stripMargin

  val exampleCreditCard = CC("12345", "7890", UUID.randomUUID(), UUID.randomUUID(), CreditCardTypes.Mastercard, 8, 2020, "Kurt Vonnegut", Currency.CAD, None, UUID.randomUUID(), Some(BillingLocation("US", None)))



  val allSupportedOptionalSchema = kvp("boolean", boolean.optional) ::
    kvp("int", int(iv.between(0,10)).optional) ::
    kvp("long", long(lv.min(0)).optional) ::
    kvp("listOfInt", list(int).optional) ::
    kvp("string", string(sv.min(0), sv.words).optional) ::
    kvp("float", float(fv.max(100)).optional) ::
    kvp("short", short(shv.max(100)).optional) ::
    kvp("double", double(dv.min(0)).optional) ::
    kvp("byteArray", byteArray.optional) ::
    kvp("localDate", localDate.optional) ::
    kvp("localDateTime", localDateTime.optional) ::
    kvp("uuid", uuid.optional) ::
    kvp("enumeration", enumeration[Currency.type, Currency.Value](Currency).optional) ::
    kvp("bigDecimal", bigDecimal(bdv.max(BigDecimal(100))).optional) ::
    kvp("either", either(string(sv.words), int).optional) ::
    kvpNil

  case class AllSupportedOptional(
                                   b: Option[Boolean],
                                   i: Option[Int],
                                   l: Option[Long],
                                   ls: Option[List[Int]],
                                   str: Option[String],
                                   f: Option[Float],
                                   s: Option[Short],
                                   d: Option[Double],
                                   ba: Option[Array[Byte]],
                                   ld: Option[LocalDate],
                                   ldt: Option[LocalDateTime],
                                   uuid: Option[UUID],
                                   currency: Option[Currency.Value],
                                   bd: Option[BigDecimal],
                                   e: Option[Either[String,Int]]
                                 )

  val allSupportedSchema =
        kvp("boolean", boolean) ::
        kvp("int", int(iv.between(0,10))) ::
        kvp("long", long(lv.min(0))) ::
        kvp("listOfInt", list(int)) ::
        kvp("string", string(sv.min(0), sv.words)) ::
        kvp("float", float(fv.max(100))) ::
        kvp("short", short(shv.max(100))) ::
        kvp("double", double(dv.min(0))) ::
        kvp("byteArray", byteArray) ::
        kvp("localDate", localDate) ::
        kvp("localDateTime", localDateTime) ::
        kvp("uuid", uuid) ::
        kvp("enumeration", enumeration[Currency.type, Currency.Value](Currency)) ::
        kvp("bigDecimal", bigDecimal(bdv.max(BigDecimal(100)))) ::
        kvp("either", either(string(sv.words), int)) ::
        kvp("child", allSupportedOptionalSchema.convert[AllSupportedOptional]) ::
        KvpNil



  case class AllSupported(
                         b: Boolean,
                         i: Int,
                         l: Long,
                         ls: List[Int],
                         str: String,
                         f: Float,
                         s: Short,
                         d: Double,
                         ba: Array[Byte],
                         ld: LocalDate,
                         ldt: LocalDateTime,
                         uuid: UUID,
                         currency: Currency.Value,
                         bd: BigDecimal,
                         either: Either[String,Int],
                         child: AllSupportedOptional
                         )


  val allSupportCaseClass = allSupportedSchema.convert[AllSupported]



}
