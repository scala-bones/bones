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
    override def produceObject(key: Key): Either[WrongTypeError[String], Option[JsonProducer]] = Right(None)
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

    case class Location(countryIso: String, postalCode: Option[String])
    case class User(username: String, pass: String, message: Option[String], location: Location)

    val rvp = new NoneJsonProducer {
      override def produceString(key: Key): Either[WrongTypeError[String], Option[String]] = key match {
        case StringKey(name) => {
          name match {
            case "username" => Right(Some("Thisisusername"))
            case "password" => Right(Some("thisispassword"))
            case "message" => Right(None)
            case "role" => Right(Some("employee"))
            case "countryIso" => Right(Some("US"))
            case "postalCode" => Right(Some("28791"))
          }
        }
        case RootKey => ???
      }

      override def produceObject(key: Key): Either[WrongTypeError[String], Option[JsonProducer]] = Right(Some(this))
    }

    /*
    { "username" : "travis", "password" : "blah", "role": "manager" }
     */
    val prog = Soyo.obj4(
      key("username").string.alphanum(),
      key("password").string(),
      key("message").string().optional(),
      key("location").obj2(
        key("countryIso").string(),
        key("postalCode").string().optional(),
        Location(_:String,_:Option[String])
      ),
      User(_:String,_:String,_:Option[String],_:Location)
    )

    import cats.implicits._
    val userPass = prog.foldMap[FromProducer](defaultCompiler).apply(rvp)


    assert( userPass == Valid( User("Thisisusername", "thisispassword", None, Location("US", Some("28791")))) )


  }

  test("alphanum passes") {

    val alpha = key("username").string.alphanum.optional

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
