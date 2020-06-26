package com.bones.schemas

import java.time.{Instant, LocalDate, LocalDateTime, Month}
import java.util.UUID

import com.bones.data.custom.{AllCustomAlgebras, ScalaCoreValue}
import com.bones.schemas.CovSumTypeExample.{Digital, MusicMedium}
import com.bones.syntax._
import com.bones.schemas.Schemas.AllSupported
import com.bones.validation.ValidationDefinition.ValidationOp
import shapeless.{:+:, CNil, HNil, Inl, Inr}

object CovSchemas {

  object CreditCardType extends Enumeration {
    type CreditCardTypes = Value
    val Visa, Mastercard, Amex, Discover = Value

  }

  case class BillingLocation(countryIso: String, zipCode: Option[String])

  // Scala Enumeration Example
  object Currency extends Enumeration {
    val USD = Value("USD")
    val CAD = Value("CAD")
    val GBP = Value("GBP")
  }

  case class CC(
    firstSix: String,
    lastFour: String,
    uuid: UUID,
    token: UUID,
    ccType: CreditCardType.Value,
    expMonth: Long,
    expYear: Long,
    cardholder: String,
    currency: Currency.Value,
    deletedAt: Option[LocalDateTime],
    lastModifiedRequest: UUID,
    billingLocation: Option[BillingLocation])

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
    ("expMonth", long(lv.between(1, 12))) ::
      ("expYear", long(lv.between(1950, 9999))) ::
      kvpNil
  ).validate(HasNotExpired)

  val ccTypeValue = enumeration[CreditCardType.type, CreditCardType.Value](CreditCardType)

  // Here we are defining our expected input data.  This definition will drive the interpreters.
  val ccObj = (
    ("firstSix", string(sv.length(6), sv.matchesRegex("[0-9]{6}".r))) ::
      ("lastFour", string(sv.length(4), sv.matchesRegex("[0-9]{4}".r))) ::
      ("uuid", uuid) ::
      ("token", uuid) ::
      ("ccType", ccTypeValue) ::
      kvpNil
  ) ::: ccExp ::: (
    ("cardHolder", string(sv.words)) ::
      ("currencyIso", enumeration[Currency.type, Currency.Value](Currency)) ::
      ("deletedAt", localDateTime.optional) :<:
      ("lastModifiedRequest", uuid) ::
      (
      "billingLocation",
      (
        ("countryIso", string(sv.validVector(isoList))) ::
          ("zipCode", string(sv.max(10)).optional) :<:
          kvpNil
      ).convert[BillingLocation].optional) :<:
      kvpNil
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
      |  "ccType" : "Mastercard",
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

  val exampleCreditCard = CC(
    "12345",
    "7890",
    UUID.randomUUID(),
    UUID.randomUUID(),
    CreditCardType.Mastercard,
    8,
    2020,
    "Kurt Vonnegut",
    Currency.CAD,
    None,
    UUID.randomUUID(),
    Some(BillingLocation("US", None))
  )

  val allSupportedOptionalSchema = ("boolean", boolean.optional) :<:
    ("int", int(iv.between(0, 10)).optional) :<:
    ("long", long(lv.min(0)).optional) :<:
    ("listOfInt", list(int).optional) :<:
    ("string", string(sv.min(0), sv.words).optional) :<:
    ("float", float(fv.max(100)).optional) :<:
    ("short", short(shv.max(100)).optional) :<:
    ("double", double(dv.min(0)).optional) :<:
    ("byteArray", byteArray.optional) :<:
    ("localDate", localDate(jt_ld.min(LocalDate.of(1800, 1, 1))).optional) :<:
    ("localDateTime",
    localDateTime(jt_ldt.min(LocalDateTime.of(1800, Month.JANUARY, 1, 0, 0))).optional) :<:
    ("uuid", uuid.optional) :<:
    ("enumeration", enumeration[Currency.type, Currency.Value](Currency).optional) :<:
    ("bigDecimal", bigDecimal(bdv.max(BigDecimal(100))).optional) :<:
    ("either", either(string(sv.words), int).optional) :<:
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
    e: Option[Either[String, Int]]
  )

  val allSupportedSchema =
    ("boolean", boolean) ::
      ("int", int(iv.between(0, 10))) ::
      ("long", long(lv.min(0))) ::
      ("listOfInt", list(int)) :<:
      ("string", string(sv.min(0), sv.words)) ::
      ("float", float(fv.max(100))) ::
      ("short", short(shv.max(100))) ::
      ("double", double(dv.min(0))) ::
      ("byteArray", byteArray) ::
      ("localDate", localDate(jt_ld.min(LocalDate.of(1800, 1, 1)))) ::
      ("localDateTime", localDateTime(jt_ldt.min(LocalDateTime.of(1800, Month.JANUARY, 1, 0, 0)))) ::
      ("uuid", uuid) ::
      ("enumeration", enumeration[Currency.type, Currency.Value](Currency)) ::
      ("bigDecimal", bigDecimal(bdv.max(BigDecimal(100)))) ::
      ("eitherField", either(string(sv.words), int)) :<:
      ("child", allSupportedOptionalSchema.convert[AllSupportedOptional]) :<:
      ("media", MusicMedium.bonesSchema) :<:
      ("int2", int(iv.between(Int.MinValue, Int.MinValue))) ::
      kvpNil

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
    either: Either[String, Int],
    child: AllSupportedOptional,
    media: MusicMedium,
    int2: Int
  ) {

    /** Adds special test to accommodate Array[Byte] */
    def fancyEquals(that: AllSupported): Boolean = {
      val nullBa = Array[Byte]()

      //Arrays seem to only be equal when they reference the same object, so let's remove them form the whole object copy
      val newThis = this.copy(ba = nullBa).copy(child = this.child.copy(ba = None))
      val newThat = that.copy(ba = nullBa).copy(child = that.child.copy(ba = None))
      val thisEqualsThat = newThis == newThat
      val arraysAreEqual = java.util.Arrays.equals(this.ba, that.ba)
      val childArrayIsEqual = (this.child.ba, that.child.ba) match {
        case (None, None)         => true
        case (Some(a1), Some(a2)) => java.util.Arrays.equals(a1, a2)
        case _                    => false
      }

      thisEqualsThat &&
      arraysAreEqual &&
      childArrayIsEqual
    }

  }

  val allSupportCaseClass = allSupportedSchema.convert[AllSupported]

  val allSupporedOptionalInstance = AllSupportedOptional(
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None)

  val allSupportedInstance = AllSupported(
    true,
    0,
    0l,
    List(1),
    "str",
    3.3f,
    3,
    3.4,
    "hello".getBytes,
    LocalDate.now,
    LocalDateTime.now,
    UUID.randomUUID(),
    Currency.CAD,
    BigDecimal("5.0"),
    Left("Hello"),
    allSupporedOptionalInstance,
    Digital("em pee three", "mp3"),
    7
  )


}
