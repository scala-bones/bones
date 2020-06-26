package com.bones.scalacheck.custom

import com.bones.data.custom.{BigDecimalData, BooleanData, ByteArrayData, DoubleData, EnumerationData, FloatData, IntData, LongData, ScalaCoreValue, ShortData, StringData}
import com.bones.scalacheck.GenAlg
import com.bones.validation.ValidationDefinition.StringValidation.{IsAlphanumeric, Length, Lowercase, MatchesRegex, MaxLength, MinLength, Sentence, Token, Uppercase, Words}
import com.bones.validation.ValidationDefinition.{BaseValidationOp, BigDecimalValidation, DoubleValidation, FloatValidation, IntValidation, LongValidation, OrderingValidation, ShortValidation, StringValidation, ValidValue, ValidationOp, ZeroValidations}
import com.bones.validation.ValidationUtil
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalacheck.Gen.Choose
import wolfendale.scalacheck.regexp.RegexpGen

object ScalacheckScalaCoreInterpreter {

  val loremIpsumString =
    s"""
       |Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet. Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet. Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet.
       |Duis autem vel eum iriure dolor in hendrerit in vulputate velit esse molestie consequat, vel illum dolore eu feugiat nulla facilisis at vero eros et accumsan et iusto odio dignissim qui blandit praesent luptatum zzril delenit augue duis dolore te feugait nulla facilisi. Lorem ipsum dolor sit amet, consectetuer adipiscing elit, sed diam nonummy nibh euismod tincidunt ut laoreet dolore magna aliquam erat volutpat.
       |Ut wisi enim ad minim veniam, quis nostrud exerci tation ullamcorper suscipit lobortis nisl ut aliquip ex ea commodo consequat. Duis autem vel eum iriure dolor in hendrerit in vulputate velit esse molestie consequat, vel illum dolore eu feugiat nulla facilisis at vero eros et accumsan et iusto odio dignissim qui blandit praesent luptatum zzril delenit augue duis dolore te feugait nulla facilisi.
       |Nam liber tempor cum soluta nobis eleifend option congue nihil imperdiet doming id quod mazim placerat facer possim assum. Lorem ipsum dolor sit amet, consectetuer adipiscing elit, sed diam nonummy nibh euismod tincidunt ut laoreet dolore magna aliquam erat volutpat. Ut wisi enim ad minim veniam, quis nostrud exerci tation ullamcorper suscipit lobortis nisl ut aliquip ex ea commodo consequat.
       |Duis autem vel eum iriure dolor in hendrerit in vulputate velit esse molestie consequat, vel illum dolore eu feugiat nulla facilisis.
       |At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet. Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet. Lorem ipsum dolor sit amet, consetetur sadipscing elitr, At accusam aliquyam diam diam dolore dolores duo eirmod eos erat, et nonumy sed tempor et et invidunt justo labore Stet clita ea et gubergren, kasd magna no rebum. sanctus sea sed takimata ut vero voluptua. est Lorem ipsum dolor sit amet. Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat.
       |Consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet. Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet. Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus
     """.stripMargin

  val loremIpsumWords = loremIpsumString.split(' ').map(_.trim.replaceAll("[,.]", "")).toSeq
  val wordsGen = Gen.oneOf(loremIpsumWords)

  val loremIpsumSentences = loremIpsumString.split('.').map(_.trim).toSeq
  val sentencesGen = Gen.oneOf(loremIpsumSentences)

  val creditCardGen: Gen[String] = Gen.oneOf("5454545454545454", "4111111111111111")

  def stringConstraints(ops: List[ValidationOp[String]]): Gen[String] = {

    val regex: Gen[String] =
      ops.to(LazyList)
        .collectFirst {
          case Words =>
            for {
              len <- Gen.choose(0, 20)
              str <- Gen.listOfN(len, wordsGen)
            } yield str.mkString(" ")
          case Sentence          => sentencesGen
          case IsAlphanumeric    => Gen.alphaNumStr
          case MatchesRegex(r)   => RegexpGen.from(r.pattern.pattern())
          case Uppercase         => Gen.alphaUpperStr
          case Token             => RegexpGen.from(StringValidation.tokenRegex.pattern.pattern())
          case Lowercase         => Gen.alphaLowerStr
          case ValidValue(valid) => Gen.oneOf(valid)
        }
        .getOrElse(Gen.asciiStr)

    val regexWithMin: Gen[String] = ops
      .collectFirst {
        case MinLength(i) => i
        case Length(l)    => l
      }
      .map(min => {
        regex.flatMap(str => {

          def appendUntilValid(gen: Gen[String], minLength: Int): Gen[String] = {
            gen.flatMap(str => {
              if (str.length >= minLength) Gen.const(str)
              else {
                appendUntilValid(gen, minLength - str.length).map(s2 => str + s2)
              }
            })
          }

          appendUntilValid(regex, min)
        })
      })
      .getOrElse(regex)

    val regexWithMax = ops
      .collectFirst {
        case MaxLength(i) => i
      }
      .map(max => {
        regexWithMin.map(str => if (str.length <= max) str else str.substring(0, max))
      })
      .getOrElse(regexWithMin)

    ops
      .collectFirst { case Length(l) => l }
      .map(max => {
        regexWithMin.map(str => if (str.length == max) str else str.substring(0, max))
      })
      .getOrElse(regexWithMax)

  }

  case class NumberConstraints[N](
                                   valid: Option[List[N]],
                                   invalid: Option[List[N]],
                                   max: Option[N],
                                   maxIsInclusive: Boolean,
                                   min: Option[N],
                                   minIsInclusive: Boolean)


  /** Uses known Bones validations to create a Number which passes validation */
  def validationConstraints[A](
                                ops: List[ValidationOp[A]],
                                vop: BaseValidationOp[A] with OrderingValidation[A] with ZeroValidations[A],
                                incrementF: A => A,
                                decrementF: A => A,
                                min: A,
                                max: A)(implicit c: Gen.Choose[A]): Gen[A] = {
    val constraints = ops.foldLeft(NumberConstraints[A](None, None, None, true, None, true)) {
      (nc, op) =>
        op match {
          case vop.Between(minV, maxV) =>
            nc.copy(
              max = Some(maxV),
              min = Some(minV),
              maxIsInclusive = true,
              minIsInclusive = true)
          case vop.Greater(min) => nc.copy(min = Some(min), minIsInclusive = false)
          case vop.Less(max)    => nc.copy(max = Some(max), maxIsInclusive = false)
          case vop.Positive     => nc.copy(min = Some(vop.zero), minIsInclusive = false)
          case vop.Max(max)     => nc.copy(max = Some(max), maxIsInclusive = true)
          case vop.Min(min)     => nc.copy(min = Some(min), minIsInclusive = true)
          case vop.Negative     => nc.copy(max = Some(vop.zero), maxIsInclusive = false)
        }
    }

    constraints.valid
      .map(v => Gen.oneOf(v))
      .getOrElse({
        val minValue = constraints.min
          .map(i => if (constraints.minIsInclusive) i else incrementF(i))
          .getOrElse(min)
        val maxValue = constraints.max
          .map(i => if (constraints.maxIsInclusive) i else decrementF(i))
          .getOrElse(max)
        c.choose(minValue, maxValue)
        //        Gen.choose[A](minValue, maxValue)
      })
      .suchThat(i => ValidationUtil.validate(ops)(i, List.empty).isRight)

  }

}

trait ScalacheckScalaCoreInterpreter extends GenAlg[ScalaCoreValue] {

  import ScalacheckScalaCoreInterpreter._

  override def gen[A](ag: ScalaCoreValue[A]): Gen[A] =
    ag match {
      case ob: BooleanData => arbitrary[Boolean]
      case rs: StringData  => stringConstraints(rs.validations)
      case sd: ShortData => {
        val one: Short = 1
        validationConstraints[Short](
          sd.validations,
          ShortValidation,
          s => (s + 1).toShort,
          s => (s - 1).toShort,
          Short.MinValue,
          Short.MaxValue)
      }
      case id: IntData =>
        validationConstraints[Int](
          id.validations,
          IntValidation,
          _ + 1,
          _ - 1,
          Int.MinValue,
          Int.MaxValue)
      case ri: LongData =>
        validationConstraints[Long](
          ri.validations,
          LongValidation,
          _ + 1,
          _ + 1,
          Long.MinValue,
          Long.MaxValue)
      case fd: FloatData =>
        validationConstraints[Float](
          fd.validations,
          FloatValidation,
          _ + .0001f,
          _ - 0001f,
          Float.MinValue,
          Float.MaxValue)
      case id: DoubleData =>
        validationConstraints[Double](
          id.validations,
          DoubleValidation,
          _ + .00001,
          _ - .00001,
          Double.MinValue,
          Double.MaxValue)
      case bd: BigDecimalData =>
        validationConstraints[BigDecimal](
          bd.validations,
          BigDecimalValidation,
          _ + BigDecimal(".00000001"),
          _ - BigDecimal(".00000001"),
          BigDecimal(Double.MinValue),
          BigDecimal(Double.MaxValue))(
          Choose.xmap[Double, BigDecimal](d => BigDecimal(d), _.toDouble)
        )
      case ba: ByteArrayData => arbitrary[Array[Byte]]
      case esd: EnumerationData[e, A] => {
        Gen.oneOf(esd.enumeration.values.toSeq.map(_.asInstanceOf[A]))
      }
    }
}
