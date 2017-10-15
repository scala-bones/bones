package com.gaia.soy

import com.gaia.soy.Soyo.{Ap2, Ap4}
import com.gaia.soy.StringValidation.Max
import org.scalatest.FunSuite

class ExampleTest extends FunSuite {


  test("groups") {

    case class User(username: String, pass: String, message: Option[String], role: Option[String])

    val rvp = new RawValueProducer {
      override def produce[X](key: Key): Either[WrongTypeError[String], Option[String]] = key.name match {
        case "username" => Right(Some("Thisisusername"))
        case "password" => Right(Some("thisispassword"))
        case "message" => Right(None)
        case "role" => Right(Some("employee"))
      }
    }

    /*
    { "username" : "travis", "password" : "blah", "role": "manager" }
     */
    val userPass = Ap4(
      key("username").string.alphanum(),
      key("password").string(),
      key("message").string().optional(),
      key("role").string().valid("manager", "employee").optional()
    ).map(User.tupled).extract(rvp)

    assert( userPass == Right( User("Thisisusername", "thisispassword", None, Some("employee"))) )


  }

  test("alphanum passes") {

    val alpha = key("username").string.alphanum.optional

    val validInput = new RawValueProducer {
      override def produce[X](key: Key): Either[WrongTypeError[String], Option[String]] = {
        Right(Some("thisisalphanum"))
//        Right(Coproduct[JSON]("thisisalphanum").select[String])
      }
    }


    val invalidInput = new RawValueProducer {
      override def produce[X](key: Key): Either[WrongTypeError[String], Option[String]] = Right(Some("Not(&*&Valid"))
    }

    assert( alpha.extract(validInput) == Right(Some("thisisalphanum")))
    assert( alpha.extract(invalidInput).isLeft)




  }

  test("passes multiple tests") {
    case class User(username: String, password: String, accessToken: Either[String, Int], birthyear: Int, email: String)

    val x = key("username").string.alphanum.min(3).max(7).optional()

    val validInput = new RawValueProducer {
      override def produce[X](key: Key): Either[WrongTypeError[String], Option[String]] = Right(Some("valid"))
    }

    val failsOne = new RawValueProducer {
      override def produce[X](key: Key): Either[WrongTypeError[String], Option[String]] = Right(Some("thisistoolong"))
    }

    val failsTwo = new RawValueProducer {
      override def produce[X](key: Key): Either[WrongTypeError[String], Option[String]] = Right(Some("$3"))
    }

    assert( x.extract(validInput) == Right(Some("valid")))


    x.extract(failsOne) match {
      case Left(nel) => {
        assert( nel.size == 1)
        nel.head match {
          case ValidationError(k, exOp, i) => assert(exOp.isInstanceOf[Max])
          case x => fail("Expected ValidationError, received: " + x)
        }
      }
      case x => fail("Expected Invalid, received:" + x)
    }

    x.extract(failsTwo) match {
      case Left(nel) => {
        assert( nel.size == 2)
      }
      case x => fail("Expected Invalid, received:" + x)
    }




  }

}
