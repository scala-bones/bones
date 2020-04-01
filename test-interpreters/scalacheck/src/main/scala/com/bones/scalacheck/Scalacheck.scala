package com.bones.scalacheck

import java.time._
import java.util.UUID

import com.bones.data.custom.CNilF
import com.bones.data.{KvpCoNil, KvpCoproduct, KvpSingleValueLeft, _}
import com.bones.interpreter.KvpInterchangeFormatEncoderInterpreter.InterchangeFormatEncoder
import com.bones.syntax.NoAlgebra
import com.bones.validation.ValidationDefinition.StringValidation._
import com.bones.validation.ValidationDefinition._
import com.bones.validation.ValidationUtil
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen.Choose
import org.scalacheck._
import shapeless.{:+:, Coproduct, HList, HNil, Inl, Inr, Nat}
import wolfendale.scalacheck.regexp.RegexpGen

object GenAlg {
  /** using kind projector allows us to create a new interpreter by merging two existing interpreters.
    * see https://stackoverflow.com/a/60561575/387094
    * */
  def merge[L[_], R[_] <: Coproduct, A](li: GenAlg[L], ri: GenAlg[R]): GenAlg[Lambda[A => L[A] :+: R[A]]] =
    new GenAlg[Lambda[A => L[A] :+: R[A]]] {

      override def gen[A](lr: L[A] :+: R[A]): Gen[A] = lr match {
        case Inl(l) => li.gen(l)
        case Inr(r) => ri.gen(r)
      }
    }

  implicit class InterpreterOps[ALG[_], OUT](val base: GenAlg[ALG]) extends AnyVal {
    def ++[R[_] <: Coproduct](r: GenAlg[R]): GenAlg[Lambda[A => ALG[A] :+: R[A]]] =
      merge(base, r)
  }

  object CNilGenEncoder extends GenAlg[CNilF] {
    override def gen[A](ag: CNilF[A]): Gen[A] = sys.error("Unreachable code")
  }

}

/**
  * Implement this to support a custom Algebra.
  *
  * @tparam ALG
  */
trait GenAlg[ALG[_]] {
  def gen[A](ag: ALG[A]): Gen[A]
}

object NoAlgebraGen extends GenAlg[NoAlgebra] {
  override def gen[A](ag: NoAlgebra[A]): Gen[A] = sys.error("Unreachable code")
}

object Scalacheck extends ScalacheckBase {
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
}

trait ScalacheckBase {

  val wordsGen: Gen[String]
  val sentencesGen: Gen[String]

  implicit val chooseLocalDate = new Choose[LocalDate] {
    override def choose(min: LocalDate, max: LocalDate): Gen[LocalDate] =
      Gen.choose(min.toEpochDay, max.toEpochDay).map(LocalDate.ofEpochDay)
  }

  implicit val chooseLocalTime = new Choose[LocalTime] {
    override def choose(min: LocalTime, max: LocalTime): Gen[LocalTime] =
      Gen.choose(min.toNanoOfDay, max.toNanoOfDay).map(LocalTime.ofNanoOfDay)
  }

  implicit val chooseLocalDateTime = new Choose[LocalDateTime] {
    override def choose(min: LocalDateTime, max: LocalDateTime): Gen[LocalDateTime] =
      for {
        localDate <- Gen.choose(min.toLocalDate, max.toLocalDate)
        localTime <- {
          // time is "all day" except when date is equal to the extremes.  The we can only
          // go the min/max of the extremes
          val minTime = if (localDate == min.toLocalDate) min.toLocalTime else LocalTime.MIN
          val maxTime = if (localDate == max.toLocalDate) max.toLocalTime else LocalTime.MAX
          Gen.choose(minTime, maxTime)
        }
      } yield (LocalDateTime.of(localDate, localTime))
  }

  def genTime[A](validations: List[ValidationOp[A]], validation: BaseDateValidation[A], globalMin: A, globalMax: A, choose: Gen.Choose[A]): Gen[A] = {
    // Using calendar results in invalid leap years, so we'll use Int instead
    val min = validations
      .collectFirst({
        case validation.MinTime(min, _, _, _) => min
      })
      .getOrElse(globalMin)
    val max = validations
      .collectFirst({
        case validation.MaxTime(max, _, _, _) => max
      })
      .getOrElse(globalMax)
    choose.choose(min, max)
  }


  def createCustomGen[ALG[_], A](bonesSchema: BonesSchema[ALG, A], genAlg: GenAlg[ALG]): Gen[A] =
    bonesSchema match {
      case co: KvpCoproductConvert[ALG, c, a] => valueDefinition(co, genAlg)
      case co: HListConvert[ALG, h, n, a] => valueDefinition(co, genAlg)
    }

  def createGen[A](bonesSchema: BonesSchema[NoAlgebra, A]): Gen[A] =
    createCustomGen[NoAlgebra, A](bonesSchema, NoAlgebraGen)

  def kvpCoproduct[ALG[_], C <: Coproduct](
                                            co: KvpCoproduct[ALG, C],
                                            genAlg: GenAlg[ALG]): List[Gen[C]] = {
    co match {
      case nil: KvpCoNil[_] => List.empty
      case co: KvpSingleValueLeft[ALG, a, r]@unchecked =>
        val head = determineValueDefinition[ALG, a](co.kvpValue, genAlg).map(Inl(_))
        val tail = kvpCoproduct(co.kvpTail, genAlg).map(gen => gen.map(Inr(_)))
        head :: tail
    }
  }

  def kvpHList[ALG[_], H <: HList, HL <: Nat](
                                               group: KvpHList[ALG, H, HL],
                                               genAlg: GenAlg[ALG]): Gen[H] = {
    group match {
      case ni: KvpNil[_] => Gen.const(HNil)
      case op: KvpSingleValueHead[ALG, h, t, tl, a]@unchecked =>
        implicit val isHCons = op.isHCons
        val headGen = determineValueDefinition(op.fieldDefinition.dataDefinition, genAlg)
        val tailGen = kvpHList(op.tail, genAlg)
        val result: Gen[H] = for {
          head <- headGen
          tail <- tailGen
        } yield {
          op.isHCons.cons(head, tail)
        }
        result
      case op: KvpHListHead[ALG, a, al, h, hl, t, tl]@unchecked =>
        implicit val prepend = op.prepend
        val headGen = kvpHList(op.head, genAlg)
        val tailGen = kvpHList(op.tail, genAlg)
        for {
          head <- headGen
          tail <- tailGen
        } yield {
          head ::: tail
        }
      case op: KvpConcreteTypeHead[ALG, a, ht, nt]@unchecked =>
        val headGen = fromCustomSchema(op.bonesSchema, genAlg)
        val tailGen = kvpHList(op.tail, genAlg)
        for {
          a <- headGen
          tail <- tailGen
        } yield {
          op.isHCons.cons(a, tail)
        }
    }
  }

  def fromBonesSchema[A](bonesSchema: BonesSchema[NoAlgebra, A]): Gen[A] =
    fromCustomSchema(bonesSchema, NoAlgebraGen)

  def fromCustomSchema[ALG[_], A](bonesSchema: BonesSchema[ALG, A], genAlg: GenAlg[ALG]): Gen[A] = {
    bonesSchema match {
      case hl: HListConvert[ALG, h, n, a] => kvpHList(hl.from, genAlg).map(hl.fHtoA)
      case co: KvpCoproductConvert[ALG, c, a] => {
        val gens = kvpCoproduct(co.from, genAlg).map(genList => genList.map(co.cToA)).map((1, _))
        Gen.frequency(gens: _*)
      }
    }
  }

  def determineValueDefinition[ALG[_], A](
                                           value: Either[KvpValue[A], ALG[A]],
                                           genAlg: GenAlg[ALG]): Gen[A] = {
    value match {
      case Left(kvp) => valueDefinition(kvp, genAlg)
      case Right(alg) => genAlg.gen(alg)
    }
  }

  def valueDefinition[ALG[_], A](fgo: KvpValue[A], genAlg: GenAlg[ALG]): Gen[A] =
    fgo match {
      case op: OptionalKvpValueDefinition[ALG, a]@unchecked => {
        val optionalGen = determineValueDefinition(op.valueDefinitionOp, genAlg).map(Some(_))
        Gen.frequency(
          (9, optionalGen),
          (1, None)
        )
      }
      case ob: BooleanData => arbitrary[Boolean]
      case rs: StringData => stringConstraints(rs.validations)
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
      case uu: UuidData => arbitrary[UUID]
      case dd: LocalDateData =>
        genTime(dd.validations, LocalDateValidationInstances, LocalDate.MIN, LocalDate.MAX, chooseLocalDate)
      case dd: LocalDateTimeData =>
        genTime(
          dd.validations,
          LocalDateTimeValidationInstances,
          LocalDateTime.of(LocalDate.ofEpochDay(Int.MinValue), LocalTime.MIN), // toEpochMilli in LocalDateTime doesn't work if the value is outside of the Int range
          LocalDateTime.of(LocalDate.ofEpochDay(Int.MaxValue), LocalTime.MAX),
          chooseLocalDateTime
        )
      case dt: LocalTimeData =>
        genTime(dt.validations, LocalTimeValidationInstances, LocalTime.MIN, LocalTime.MAX, chooseLocalTime)
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
      case ld: ListData[ALG, t]@unchecked =>
        implicit val elemGenerator = determineValueDefinition(ld.tDefinition, genAlg)
        for {
          numElems <- Gen.choose(1, 500)
          elem <- Gen.listOfN(numElems, elemGenerator)
        } yield elem
      case ed: EitherData[ALG, a, b]@unchecked => {
        val left = determineValueDefinition(ed.definitionA, genAlg).map(Left(_))
        val right = determineValueDefinition(ed.definitionB, genAlg).map(Right(_))
        Gen.frequency((1, left), (1, right))
      }
      case ba: ByteArrayData => arbitrary[Array[Byte]]
      case esd: EnumerationData[e, A] => {
        Gen.oneOf(esd.enumeration.values.toSeq.map(_.asInstanceOf[A]))
      }
      case kvp: KvpHListValue[ALG, h, hl]@unchecked =>
        kvpHList(kvp.kvpHList, genAlg).map(_.asInstanceOf[A])
      case co: KvpCoproductValue[ALG, c]@unchecked =>
        // Get a list of coproduct and gen them with equal frequency (1)
        val gens = kvpCoproduct(co.kvpCoproduct, genAlg).map(_.map(_.asInstanceOf[A])).map((1, _))
        Gen.frequency(gens: _*)
      case x: HListConvert[ALG, a, al, b]@unchecked =>
        kvpHList(x.from, genAlg).map(hList => x.fHtoA(hList))
      case co: KvpCoproductConvert[ALG, c, a]@unchecked =>
        val gens = kvpCoproduct(co.from, genAlg).map((1, _))
        Gen.frequency(gens: _*).map(coproduct => co.cToA(coproduct))
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
          case vop.Less(max) => nc.copy(max = Some(max), maxIsInclusive = false)
          case vop.Positive => nc.copy(min = Some(vop.zero), minIsInclusive = false)
          case vop.Max(max) => nc.copy(max = Some(max), maxIsInclusive = true)
          case vop.Min(min) => nc.copy(min = Some(min), minIsInclusive = true)
          case vop.Negative => nc.copy(max = Some(vop.zero), maxIsInclusive = false)
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

  val creditCardGen: Gen[String] = Gen.oneOf("5454545454545454", "4111111111111111")

  def stringConstraints(ops: List[ValidationOp[String]]): Gen[String] = {

    val regex: Gen[String] =
      ops.toStream
        .collectFirst {
          case Words =>
            for {
              len <- Gen.choose(0, 20)
              str <- Gen.listOfN(len, wordsGen)
            } yield str.mkString(" ")
          case Sentence => sentencesGen
          case IsAlphanumeric => Gen.alphaNumStr
          case MatchesRegex(r) => RegexpGen.from(r.pattern.pattern())
          case Uppercase => Gen.alphaUpperStr
          case Token => RegexpGen.from(StringValidation.tokenRegex.pattern.pattern())
          case Lowercase => Gen.alphaLowerStr
          case ValidValue(valid) => Gen.oneOf(valid)
        }
        .getOrElse(Gen.asciiStr)

    val regexWithMin: Gen[String] = ops
      .collectFirst {
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


}
