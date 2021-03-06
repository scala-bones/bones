package com.bones.data.values

import java.net.{URI, URL}
import java.util.UUID

import com.bones.PrimitiveValue
import com.bones.data.values.CustomStringValue._
import com.bones.validation.ValidationDefinition.ValidationOp
import com.bones.validation.ValidationUtil
import shapeless.Coproduct
import shapeless.ops.coproduct.Inject

import scala.util.Try
import scala.util.matching.Regex

object CustomStringValue {

  object EmailDataValidationOp extends ValidationOp[String] {
    // From https://stackoverflow.com/questions/13912597/validate-email-one-liner-in-scala
    val emailRegex: Regex =
      """^[a-zA-Z0-9.!#$%&'*+/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$""".r

    override val isValid: String => Boolean =
      emailRegex.findFirstMatchIn(_).isDefined

    override def defaultError(t: String): String = s"$t is not a valid email"

    override val description: String = "email"
  }

  object GuidDataValidationOp extends ValidationOp[String] {

    val isValid: String => Boolean = str =>
      Try {
        UUID.fromString(str)
      }.isSuccess

    override def defaultError(t: String): String = s"$t is not a GUID"

    override def description: String = "be a GUID"
  }

  object CreditCardValidationOp extends ValidationOp[String] {

    override def isValid: String => Boolean =
      input => ValidationUtil.luhnCheck(10, input)

    override def defaultError(t: String): String =
      s"$t is not a valid credit card number"

    override def description: String = "valid credit card number"
  }

  object HexStringValidationOp extends ValidationOp[String] {
    val upperHexRegex: Regex = "^[0-9A-F]+$".r
    val lowerHexRegex: Regex = "^[0-9a-f]+$".r

    override val isValid: String => Boolean = input =>
      upperHexRegex.findFirstMatchIn(input).isDefined || lowerHexRegex
        .findFirstMatchIn(input)
        .isDefined

    override def defaultError(t: String): String = s"$t is not hexadecimal"

    override val description: String = "hexadecimal"
  }

  object Base64ValidationOp extends ValidationOp[String] {
    val base64Regex: Regex =
      "^(?:[A-Za-z0-9+/]{4})*(?:[A-Za-z0-9+/]{2}==|[A-Za-z0-9+/]{3}=)?$".r

    override val isValid: String => Boolean =
      input => base64Regex.findFirstMatchIn(input.trim).isDefined

    override def defaultError(t: String): String = s"$t is not Base64"

    override val description: String = "Base64"
  }

  object HostnameValidationOp extends ValidationOp[String] {
    val hostnameRegex: Regex =
      "^(([a-zA-Z0-9]|[a-zA-Z0-9][a-zA-Z0-9\\-]*[a-zA-Z0-9])\\.)*([A-Za-z0-9]|[A-Za-z0-9][A-Za-z0-9\\-]*[A-Za-z0-9])$".r

    override val isValid: String => Boolean =
      hostnameRegex.findFirstMatchIn(_).isDefined

    override def defaultError(t: String): String = s"$t is not a RFC1123 hostname"

    override val description: String = "RFC1123 hostname"
  }

  object UriValidationOp extends ValidationOp[String] {
    override val isValid: String => Boolean =
      input =>
        Try {
          URI.create(input)
        }.isSuccess

    override def defaultError(t: String): String = s"$t is not a well formed URI"

    override val description: String = "URI"
  }

  object UrlValidationOp extends ValidationOp[String] {
    override val isValid: String => Boolean =
      input =>
        Try {
          new URL(input) // MalformedURLException
        }.isSuccess

    override def defaultError(t: String): String = s"$t is not a well formed URL"

    override val description: String = "URI"
  }

  object Ipv4ValidationOp extends ValidationOp[String] {

    val ipv4Regex: Regex =
      "^(([0-9]|[1-9][0-9]|1[0-9]{2}|2[0-4][0-9]|25[0-5])\\.){3}([0-9]|[1-9][0-9]|1[0-9]{2}|2[0-4][0-9]|25[0-5])$".r

    override val isValid: String => Boolean =
      ipv4Regex.findFirstMatchIn(_).isDefined

    override def defaultError(t: String): String = s"$t is not an ipv4"

    override val description: String = "IPv4"
  }

  object Ipv6ValidationOp extends ValidationOp[String] {
    // https://stackoverflow.com/a/17871737/387094

    val ip46Regex: Regex =
      "(([0-9a-fA-F]{1,4}:){7}[0-9a-fA-F]{1,4}|([0-9a-fA-F]{1,4}:){1,7}:|([0-9a-fA-F]{1,4}:){1,6}:[0-9a-fA-F]{1,4}|([0-9a-fA-F]{1,4}:){1,5}(:[0-9a-fA-F]{1,4}){1,2}|([0-9a-fA-F]{1,4}:){1,4}(:[0-9a-fA-F]{1,4}){1,3}|([0-9a-fA-F]{1,4}:){1,3}(:[0-9a-fA-F]{1,4}){1,4}|([0-9a-fA-F]{1,4}:){1,2}(:[0-9a-fA-F]{1,4}){1,5}|[0-9a-fA-F]{1,4}:((:[0-9a-fA-F]{1,4}){1,6})|:((:[0-9a-fA-F]{1,4}){1,7}|:)|fe80:(:[0-9a-fA-F]{0,4}){0,4}%[0-9a-zA-Z]+|::(ffff(:0{1,4})?:)?((25[0-5]|(2[0-4]|1?[0-9])?[0-9])\\.){3}(25[0-5]|(2[0-4]|1?[0-9])?[0-9])|([0-9a-fA-F]{1,4}:){1,4}:((25[0-5]|(2[0-4]|1?[0-9])?[0-9])\\.){3}(25[0-5]|(2[0-4]|1?[0-9])?[0-9]))".r

    override val isValid: String => Boolean =
      ip46Regex.findFirstMatchIn(_).isDefined

    override def defaultError(t: String): String = s"$t is not an ipv6"

    override val description: String = "IPv6"
  }

}

sealed abstract class CustomStringValue[A] extends PrimitiveValue[String] {
  override val typeName: String = "String"
  val validations: List[ValidationOp[String]]

  val customValidation: ValidationOp[String]

  val example: String
  val description: String

}

final case class EmailData(validations: List[ValidationOp[String]])
    extends CustomStringValue[String] {

  override val customValidation: ValidationOp[String] = CustomStringValue.EmailDataValidationOp
  override val example: String = "john.doe@example.com"
  override val description: String = "Email"
}

final case class GuidData(validations: List[ValidationOp[String]])
    extends CustomStringValue[String] {
  override val customValidation: ValidationOp[String] = CustomStringValue.GuidDataValidationOp

  val exampleUuid = "322dd565-0a28-4959-9b7e-42ba84149870"
  override val example: String = exampleUuid
  override val description: String = "Globally Unique Identifier (GUID)"
}

final case class CreditCardData(validations: List[ValidationOp[String]])
    extends CustomStringValue[String] {
  override val customValidation: ValidationOp[String] = CustomStringValue.CreditCardValidationOp
  override val example: String = "5454545454545454"
  override val description: String = "Credit Card Number"
}

final case class HexStringData(validations: List[ValidationOp[String]])
    extends CustomStringValue[String] {
  override val customValidation: ValidationOp[String] = CustomStringValue.HexStringValidationOp
  override val example: String = "0123456789abcdef"
  override val description: String = "Hex String"
}

final case class Base64Data(validations: List[ValidationOp[String]])
    extends CustomStringValue[String] {
  override val customValidation: ValidationOp[String] = CustomStringValue.Base64ValidationOp
  override val example: String = "A1B2C3E4F567890$"
  override val description: String = "Base64"
}

final case class HostnameData(validations: List[ValidationOp[String]])
    extends CustomStringValue[String] {
  override val customValidation: ValidationOp[String] = CustomStringValue.HostnameValidationOp
  override val example: String = "www.example.com"
  override val description: String = "Hostname"
}

final case class UriData(validations: List[ValidationOp[String]])
    extends CustomStringValue[String] {
  override val customValidation: ValidationOp[String] = CustomStringValue.UriValidationOp
  override val example: String =
    URI.create("http://www.math.uio.no/faq/compression-faq/part1.html").toString
  override val description: String = "Uniform Resource Identifier (URI)"

}

final case class UrlData(validations: List[ValidationOp[String]])
    extends CustomStringValue[String] {
  override val customValidation: ValidationOp[String] = CustomStringValue.UrlValidationOp
  override val example
    : String = new URL("http://www.math.uio.no/faq/compression-faq/part1.html").toExternalForm
  override val description: String = "Uniform Resource Locator (URL)"
}

final case class IpV4Data(validations: List[ValidationOp[String]])
    extends CustomStringValue[String] {
  override val customValidation: ValidationOp[String] = CustomStringValue.Ipv4ValidationOp
  override val example: String = "10.0.0.1"
  override val description: String = "Internet Protocol Version 4 (IPv4)"
}

final case class IpV6Data(validations: List[ValidationOp[String]])
    extends CustomStringValue[String] {
  override val customValidation: ValidationOp[String] = CustomStringValue.Ipv6ValidationOp
  override val example: String = "::1"
  override val description: String = "Internet Protocol Version 6 (IPv6)"
}

trait CustomStringAlgebraSugar {

  val cs_e: CustomStringValue.EmailDataValidationOp.type = EmailDataValidationOp
  val cs_g: CustomStringValue.GuidDataValidationOp.type = GuidDataValidationOp
  val cs_cc: CustomStringValue.CreditCardValidationOp.type = CreditCardValidationOp
  val cs_hex: CustomStringValue.HexStringValidationOp.type = HexStringValidationOp
  val cs_b: CustomStringValue.Base64ValidationOp.type = Base64ValidationOp
  val cs_hn: CustomStringValue.HostnameValidationOp.type = HostnameValidationOp
  val cs_uri: CustomStringValue.UriValidationOp.type = UriValidationOp
  val cs_url: CustomStringValue.UrlValidationOp.type = UrlValidationOp
  val cs_ipv4: CustomStringValue.Ipv4ValidationOp.type = Ipv4ValidationOp
  val cs_ipv6: CustomStringValue.Ipv6ValidationOp.type = Ipv6ValidationOp
}

trait BaseCustomStringValue[OUT] {

  def matchCustomStringValue[A](alg: CustomStringValue[A]): OUT = {
    alg match {
      case ed: EmailData      => emailData(ed)
      case gd: GuidData       => guidData(gd)
      case cd: CreditCardData => creditCardData(cd)
      case hd: HexStringData  => hexStringData(hd)
      case bd: Base64Data     => base64Data(bd)
      case hd: HostnameData   => hostnameData(hd)
      case ud: UriData        => uriData(ud)
      case ud: UrlData        => urlData(ud)
      case id: IpV4Data       => ip4vData(id)
      case id: IpV6Data       => ip46Data(id)
    }
  }

  def emailData(emailData: EmailData): OUT
  def guidData(guidData: GuidData): OUT
  def creditCardData(creditCardData: CreditCardData): OUT
  def hexStringData(hexStringData: HexStringData): OUT
  def base64Data(base64Data: Base64Data): OUT
  def hostnameData(hostnameData: HostnameData): OUT
  def uriData(uriData: UriData): OUT
  def urlData(urlData: UrlData): OUT
  def ip4vData(ipV4Data: IpV4Data): OUT
  def ip46Data(ipV6Data: IpV6Data): OUT

}

trait CustomStringValueSugar extends CustomStringAlgebraSugar {

  /** String must be a guid */
  def guid(validationOp: ValidationOp[String]*): GuidData = GuidData(validationOp.toList)

  def guid: GuidData = guid()

  /** String must be a valid email format */
  def email(validationOp: ValidationOp[String]*): EmailData = EmailData(validationOp.toList)

  def email: EmailData = email()

  /** String must be a valid hexadecimal String */
  def hex(validationOp: ValidationOp[String]*): HexStringData = HexStringData(validationOp.toList)

  def hex: HexStringData = hex()

  /** String must be in base64 */
  def base64(validationOp: ValidationOp[String]*): Base64Data = Base64Data(validationOp.toList)

  def base64: Base64Data = base64()

  /** String must be a hostname */
  def hostname(validationOp: ValidationOp[String]*): HostnameData =
    HostnameData(validationOp.toList)

  def hostname: HostnameData = hostname()

  /** String must be an IPv4 */
  def iPv4(validationOp: ValidationOp[String]*): IpV4Data = IpV4Data(validationOp.toList)

  def iPv4: IpV4Data = iPv4()

  /** String must be a Uri */
  def uri(validationOp: ValidationOp[String]*): UriData = UriData(validationOp.toList)

  def uri: UriData = uri()

  def url(validationOp: ValidationOp[String]*): UrlData = UrlData(validationOp.toList)

  def url: UrlData = url()

  /** String must be a valid credit card number */
  def creditCard(validationOp: ValidationOp[String]*): CreditCardData =
    CreditCardData(validationOp.toList)

  def creditCard: CreditCardData = creditCard()
}

/** Adds smart constructors to lift our GADT into a multi-algebra system */
trait CustomStringValueSugarInjected[ALG[_] <: Coproduct] extends CustomStringAlgebraSugar {

  def stringValueInject[String]: Inject[ALG[String], CustomStringValue[String]]

  /** String must be a guid */
  def guid(validationOp: ValidationOp[String]*): ALG[String] =
    stringValueInject(GuidData(validationOp.toList))

  def guid: ALG[String] = guid()

  /** String must be a valid email format */
  def email(validationOp: ValidationOp[String]*): ALG[String] =
    stringValueInject(EmailData(validationOp.toList))

  def email: ALG[String] = email()

  /** String must be a valid hexadecimal String */
  def hex(validationOp: ValidationOp[String]*): ALG[String] =
    stringValueInject(HexStringData(validationOp.toList))

  def hex: ALG[String] = hex()

  /** String must be in base64 */
  def base64(validationOp: ValidationOp[String]*): ALG[String] =
    stringValueInject(Base64Data(validationOp.toList))

  def base64: ALG[String] = base64()

  /** String must be a hostname */
  def hostname(validationOp: ValidationOp[String]*): ALG[String] =
    stringValueInject(HostnameData(validationOp.toList))

  def hostname: ALG[String] = hostname()

  /** String must be an IPv4 */
  def iPv4(validationOp: ValidationOp[String]*): ALG[String] =
    stringValueInject(IpV4Data(validationOp.toList))

  def iPv4: ALG[String] = iPv4()

  /** String must be a Uri */
  def uri(validationOp: ValidationOp[String]*): ALG[String] =
    stringValueInject(UriData(validationOp.toList))

  def uri: ALG[String] = uri()

  def url(validationOp: ValidationOp[String]*): ALG[String] =
    stringValueInject(UrlData(validationOp.toList))

  def url: ALG[String] = url()

  /** String must be a valid credit card number */
  def creditCard(validationOp: ValidationOp[String]*): ALG[String] =
    stringValueInject(CreditCardData(validationOp.toList))

  def creditCard: ALG[String] = creditCard()
}
