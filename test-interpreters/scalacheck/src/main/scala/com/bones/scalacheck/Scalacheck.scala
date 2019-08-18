package com.bones.scalacheck

import java.time.{LocalDate, LocalDateTime, ZoneId}
import java.util.{Calendar, Date, UUID}

import com.bones.data.Value._
import com.bones.validation.ValidationDefinition.StringValidation._
import com.bones.validation.ValidationDefinition._
import com.bones.validation.ValidationUtil
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck._
import shapeless.{HList, HNil, Nat}
import wolfendale.scalacheck.regexp.RegexpGen

object Scalacheck {


  def kvpHList[H <: HList, HL <: Nat](group: KvpHList[H, HL]): Gen[H] = {
    group match {
      case KvpNil => Gen.const(HNil)
      case op: KvpSingleValueHead[h, t, tl, a] =>
        implicit val isHCons = op.isHCons
        val headGen = valueDefinition(op.fieldDefinition.op)
        val tailGen = kvpHList(op.tail)
        val result: Gen[H] = for {
          head <- headGen
          tail <- tailGen
        } yield {
          op.isHCons.cons(head, tail)
        }
        result
      case op: KvpHListHead[a, al, h, hl, t, tl] =>
        implicit val prepend = op.prepend
        val headGen = kvpHList(op.head)
        val tailGen = kvpHList(op.tail)
        for {
          head <- headGen
          tail <- tailGen
        } yield {
          head ::: tail
        }
      case op: KvpConcreteTypeHead[a, ht, nt, ho, xl, xll] =>
        val headGen = valueDefinition(op.hListConvert)
        val tailGen = kvpHList(op.tail)
        for {
          a <- headGen
          tail <- tailGen
        } yield {
          op.isHCons.cons(a,tail)
        }
    }
  }

  def valueDefinition[A](fgo: KvpValue[A]): Gen[A] =
    fgo match {
      case op: OptionalKvpValueDefinition[a] => {
        val optionalGen = valueDefinition(op.valueDefinitionOp).map(Some(_))
        Gen.frequency(
          (9, optionalGen),
          (1, None)
        )
      }
      case ob: BooleanData => arbitrary[Boolean]
      case rs: StringData => stringConstraints(rs.validations)
      case sd: ShortData => {
        val one: Short = 1
        validationConstraints[Short](sd.validations, ShortValidation, s => (s + 1).toShort, Short.MinValue, Short.MaxValue)
      }
      case id: IntData => validationConstraints[Int](id.validations, IntValidation, _ + 1, Int.MinValue, Int.MaxValue)
      case ri: LongData => validationConstraints[Long](ri.validations, LongValidation, _ + 1, Long.MinValue, Long.MaxValue)
      case uu: UuidData => arbitrary[UUID]
      case dd: LocalDateData => arbitrary[Calendar]
        .map(d => LocalDate.of(d.get(Calendar.YEAR), (d.get(Calendar.MONTH) + 1), d.get(Calendar.DAY_OF_MONTH)))
      case dd: LocalDateTimeData => arbitrary[Date].map(d => LocalDateTime.ofInstant(d.toInstant, ZoneId.systemDefault()))
      case fd: FloatData => validationConstraints[Float](fd.validations, FloatValidation, _ + .0001f, Float.MinValue, Float.MaxValue)
      case id: DoubleData => validationConstraints[Double](id.validations, DoubleValidation, _ + .00001, Double.MinValue, Double.MaxValue)
      case bd: BigDecimalData => arbitrary[Double].map(d => BigDecimal(d))
        // validationConstraints[BigDecimal](bd.validations, BigDecimalValidation, _ + BigDecimal(".00000001"), BigDecimal(Double.MinValue), BigDecimal(Double.MaxValue))
      case ld: ListData[t] => ???
      case ed: EitherData[a, b] => {
        val left = valueDefinition(ed.definitionA).map(Left(_))
        val right = valueDefinition(ed.definitionB).map(Right(_))
        Gen.frequency((1, left), (1, right))
      }
      case ba: ByteArrayData => arbitrary[Array[Byte]]
      case esd: EnumerationData[e,a] => {
        Gen.oneOf(esd.enumeration.values.toSeq.map(_.asInstanceOf[A]))
      }
      case kvp: KvpHListValue[h, hl] =>
        kvpHList(kvp.kvpHList).asInstanceOf[A]
      case x: HListConvert[a, al, b] =>
        kvpHList(x.from).map(hList => x.fHtoA(hList))
      case s: SumTypeData[a, b] =>
        Gen.oneOf(s.keys).map(k => s.fab(k, List.empty).right.get)
    }


  case class NumberConstraints[N](valid: Option[List[N]],
                                  invalid: Option[List[N]],
                                  max: Option[N],
                                  maxIsInclusive: Boolean,
                                  min: Option[N],
                                  minIsInclusive: Boolean)

  /** Uses known Bones validations to create a Number which passes validation */
  def validationConstraints[A](ops: List[ValidationOp[A]],
                               vop: BaseValidationOp[A] with OrderingValidation[A],
                               incrementF: A => A,
                               min: A,
                               max: A)(implicit c: Gen.Choose[A]): Gen[A] = {
    val constraints = ops.foldLeft(NumberConstraints[A](None, None, None, true, None, true)){ (nc,op)  =>
      op match {
        case vop.Between(minV, maxV) => nc.copy(max = Some(maxV), min = Some(minV), maxIsInclusive = true, minIsInclusive = true)
        case vop.Greater(min) => nc.copy(min = Some(min), minIsInclusive = false)
        case vop.Less(max) => nc.copy(max = Some(max), maxIsInclusive = false)
        case vop.Positive => nc.copy(min = Some(vop.zero), minIsInclusive = false)
      }
    }

    constraints.valid.map(v => Gen.oneOf(v))
      .getOrElse({
        val minValue = constraints.min.map(i => if (constraints.minIsInclusive) i else incrementF(i)).getOrElse(min)
        val maxValue = constraints.max.map(i => if (constraints.maxIsInclusive) i else incrementF(i)).getOrElse(max)
        Gen.choose[A](minValue, maxValue)
      }).suchThat(i => ValidationUtil.validate(ops)(i, List.empty).isRight)

  }

  val creditCardGen: Gen[String] = Gen.oneOf("5454545454545454", "4111111111111111")
  def stringConstraints(ops: List[ValidationOp[String]]): Gen[String] = {

    val regex: Gen[String] =
      ops.toStream.collectFirst{
          case Words => for {
            len <- Gen.choose(0,20)
            str <- Gen.listOfN(len, Gen.oneOf(loremIpsumWords))
          } yield str.mkString(" ")
          case Sentence => Gen.oneOf(loremIpsumSentences)
          case IsAlphanumeric => Gen.alphaNumStr
          case MatchesRegex(r) => RegexpGen.from(r.pattern.pattern())
          case Guid => Gen.uuid.map(_.toString)
          case Uppercase => Gen.alphaUpperStr
          case CreditCard => creditCardGen
          case Token => RegexpGen.from(StringValidation.tokenRegex.pattern.pattern())
          case Email => RegexpGen.from(StringValidation.emailRegex.pattern.pattern())
          case Hex => RegexpGen.from(StringValidation.hexRegex.pattern.pattern())
          case Base64 => RegexpGen.from(StringValidation.base64Regex.pattern.pattern())
          case Hostname => RegexpGen.from(StringValidation.hostnameRegex.pattern.pattern())
          case Ipv4 => RegexpGen.from(StringValidation.ipv4Regex.pattern.pattern())
          case Lowercase => Gen.alphaLowerStr
          case ValidValue(valid) => Gen.oneOf(valid)
      }
      .getOrElse(Gen.asciiStr)

    val regexWithMin: Gen[String] = ops
      .collectFirst{
        case MinLength(i) => i
        case Length(l) => l
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
      }).getOrElse(regex)

    val regexWithMax = ops
      .collectFirst{
        case MaxLength(i) => i
      }
      .map(max => {
        regexWithMin.map(str => if (str.length <= max) str else str.substring(0,max))
      })
      .getOrElse(regexWithMin)

    ops
      .collectFirst { case Length(l) => l }
      .map(max => {
        regexWithMin.map(str => if (str.length == max) str else str.substring(0,max))
      })
      .getOrElse(regexWithMax)

  }

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

  val loremIpsumWords = loremIpsumString.split(' ').map(_.trim).toSeq
  val loremIpsumSentences = loremIpsumString.split('.').map(_.trim).toSeq

}
