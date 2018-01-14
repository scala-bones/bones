package com.ot.bones

import cats.data.Validated.{Invalid, Valid}
import com.ot.bones.everything.key
import com.ot.bones.interpreter.ExtractionInterpreter.{JsonProducer, ValidationError, WrongTypeError}
import com.ot.bones.validation.Key
import com.ot.bones.validation.StringValidation.Max
import org.scalatest.FunSuite

class ValidationTest extends FunSuite {
  abstract class NoneJsonProducer extends JsonProducer {
    override def produceBool(key: Key): Either[WrongTypeError[Boolean], Option[Boolean]] = Right(None)
    override def produceString(key: Key): Either[WrongTypeError[String], Option[String]] = Right(None)
    override def produceBigDecimal(key: Key): Either[WrongTypeError[BigDecimal], Option[BigDecimal]] = Right(None)
    override def produceInt(key: Key): Either[WrongTypeError[Int], Option[Int]] = Right(None)
    override def produceObject(key: Key): Either[WrongTypeError[JsonProducer], Option[JsonProducer]] = Right(None)
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


}
