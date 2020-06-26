package com.bones.scalacheck.custom

import java.nio.charset.StandardCharsets

import com.bones.data.custom._
import com.bones.scalacheck.GenAlg
import com.bones.validation.ValidationUtil
import org.scalacheck.Gen

class CustomStringValueInterpreter extends GenAlg[CustomStringValue] {

  def checkValid[A](ag: CustomStringValue[A]): A => Boolean =
    input =>
      ValidationUtil
        .validate(ag.customValidation :: ag.validations)(input.asInstanceOf[String], List.empty)
        .isRight

  val hostnameGen: Gen[String] = for {
    host <- ScalacheckScalaCoreInterpreter.wordsGen
    domain <- ScalacheckScalaCoreInterpreter.wordsGen
    domainSuffix <- Gen.oneOf("com", "org", "com.uk")
  } yield s"$host.$domain.$domainSuffix"

  val urlGen: Gen[String] =
    for {
      hostname <- hostnameGen
      paths <- Gen.choose(0, 10)
      dirs <- Gen.listOfN(paths, ScalacheckScalaCoreInterpreter.wordsGen)
    } yield s"https://$hostname/${dirs.mkString("/")}"

  override def gen[A](ag: CustomStringValue[A]): Gen[A] = {
    val gen: Gen[A] =
      ag match {
        case _: EmailData =>
          for {
            recipient <- ScalacheckScalaCoreInterpreter.wordsGen
            num <- Gen.choose(0, 5000).map(_.toString)
            domain <- ScalacheckScalaCoreInterpreter.wordsGen
            domainSuffix <- Gen.oneOf("com", "org", "com.uk")
          } yield s"$recipient$num@$domain.$domainSuffix"
        case _: GuidData =>
          Gen.uuid.map(_.toString)
        case _: CreditCardData =>
          Gen.oneOf(testCreditCardNumbers)
        case _: HexStringData =>
          Gen.hexStr
        case _: Base64Data =>
          ScalacheckScalaCoreInterpreter.sentencesGen.map(str => {
            java.util.Base64.getEncoder.encodeToString(str.getBytes(StandardCharsets.UTF_8))
          })
        case _: HostnameData => hostnameGen
        case uri: UriData    => urlGen.retryUntil(uri.customValidation.isValid)
        case url: UrlData    => urlGen.retryUntil(url.customValidation.isValid)
        case ip: IpV4Data =>
          Gen.listOfN(4, Gen.choose(0, 255)).map(_.mkString("."))
        case ip: IpV6Data =>
          Gen.listOfN(8, Gen.listOfN(4, Gen.hexChar).map(_.mkString)).map(_.mkString(":"))
      }

    gen.retryUntil(s => checkValid(ag)(s))

  }

  val testCreditCardNumbers = Seq(
    // VISA:
    "4716057617348823",
    "4532402181513299",
    "4532417202485743188",
    //MasterCard:
    "5432711837070944",
    "2221007144424300",
    "5293048083222293",
    // American Express (AMEX):
    "342048966584753",
    "373987846823356",
    "349388227834792",
    //Discover:
    "6011351930087532",
    "6011547165681564",
    "6011374899334565516",
    // JCB:
    "3538577025662398",
    "3528668634649447",
    "3533868645221543812",
    // Diners Club - North America:
    "5573997724103869",
    "5448374886049475",
    "5594763969846868",
    // Diners Club - Carte Blanche:
    "30455552027971",
    "30090354907532",
    "30366428261761",
    // Diners Club - International:
    "36617696797017",
    "36396679792721",
    "36988111388405",
    // Maestro:
    "5018681877138699",
    "6759864602623794",
    "6304983715374424",
    // Visa Electron:
    "4175000096857439",
    "4175008073073441",
    "4508531334907964",
    // InstaPayment:
    "6375434321527660",
    "6389763355873754",
    "6378660019698788"
  )

}
