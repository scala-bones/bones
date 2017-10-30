package com.gaia.soy

import cats.data.Validated.{Invalid, Valid}
import com.gaia.soy.StringValidation.Max
import com.gaia.soy.compiler.JsonCompiler._
import org.scalatest.FunSuite

class ExampleTest extends FunSuite {


  abstract class NoneJsonProducer extends JsonProducer {
    override def produceBoole(key: Key): Either[WrongTypeError[String], Option[Boolean]] = Right(None)
    override def produceString(key: Key): Either[WrongTypeError[String], Option[String]] = Right(None)
    override def produceBigDecimal(key: Key): Either[WrongTypeError[String], Option[BigDecimal]] = Right(None)
    override def produceInt(key: Key): Either[WrongTypeError[String], Option[Int]] = Right(None)
  }

//  test("sibling groups") {
//    case class User(username: String, pass: String, message: Option[String], role: Option[String])
//    case class Car(make: String, model: String, year: Option[String])
//
//    val rvp = new NoneJsonProducer {
//      override def produceString(key: Key): Either[WrongTypeError[String], Option[String]] = key.name match {
//        case "username" => Right(Some("Thisisusername"))
//        case "password" => Right(Some("thisispassword"))
//        case "message" => Right(None)
//        case "role" => Right(Some("employee"))
//      }
//    }
//
//    Soyo.obj2(
//      key("user").obj4(
//        key("username").string.alphanum(),
//        key("password").string(),
//        key("message").string().optional(),
//        key("role").string().valid("manager", "employee").optional(),
//        User(_,_,_,_)
//      ),
//      key("car").obj4(
//        key("make").string.alphanum(),
//        key("model").string(),
//        key("message").string().optional(),
//        key("role").string().valid("manager", "employee").optional(),
//        User(_,_,_,_)
//      )
//    )
//
//  }

  test("group") {

    case class User(username: String, pass: String, message: Option[String], role: Option[String])

    val rvp = new NoneJsonProducer {
      override def produceString(key: Key): Either[WrongTypeError[String], Option[String]] = key.name match {
        case "username" => Right(Some("Thisisusername"))
        case "password" => Right(Some("thisispassword"))
        case "message" => Right(None)
        case "role" => Right(Some("employee"))
      }
    }

    /*
    { "username" : "travis", "password" : "blah", "role": "manager" }
     */

    val prog = Soyo.obj4(
      key("username").string.alphanum(),
      key("password").string(),
      key("message").string().optional(),
      key("role").string().valid("manager", "employee").optional(),
      User(_,_,_,_)
    )

    import cats.implicits._
    val userPass = prog.foldMap[FromProducer](defaultCompiler).apply(rvp)


    assert( userPass == Valid( User("Thisisusername", "thisispassword", None, Some("employee"))) )


  }

  test("alphanum passes") {

    val alpha = key("username").string.alphanum.optional

    val validInput = new StringProducer {
      override def produceString(key: Key): Either[WrongTypeError[String], Option[String]] = {
        Right(Some("thisisalphanum"))
//        Right(Coproduct[JSON]("thisisalphanum").select[String])
      }
    }


    val invalidInput = new StringProducer {
      override def produceString(key: Key): Either[WrongTypeError[String], Option[String]] = Right(Some("Not(&*&Valid"))
    }

    assert( alpha.extract(validInput) == Valid(Some("thisisalphanum")))
    assert( alpha.extract(invalidInput).isInvalid)




  }

  test("passes multiple tests") {
    case class User(username: String, password: String, accessToken: Either[String, Int], birthyear: Int, email: String)

    val x = key("username").string.alphanum.min(3).max(7).optional()

    val validInput = new StringProducer {
      override def produceString(key: Key): Either[WrongTypeError[String], Option[String]] = Right(Some("valid"))
    }

    val failsOne = new StringProducer {
      override def produceString(key: Key): Either[WrongTypeError[String], Option[String]] = Right(Some("thisistoolong"))
    }

    val failsTwo = new StringProducer {
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
