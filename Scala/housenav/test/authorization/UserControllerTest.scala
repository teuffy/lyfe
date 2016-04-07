package authorization

import scala.concurrent.{ Await, Future }
import scala.concurrent.duration.Duration
import scala.util.Random

import org.scalacheck._
import org.scalacheck.Gen._
import org.scalatest.prop.PropertyChecks
import org.scalatestplus.play.{ OneAppPerTest, PlaySpec }

import dao.UsersDAO
import javax.inject.Singleton
import models.User
import play.api.Application
import play.api.http.Writeable
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.json.{ JsValue, Json }
import play.api.mvc.Result
import play.api.test._
import play.api.test.Helpers._

class UserControllerTest extends PlaySpec with PropertyChecks with OneAppPerTest {

  val userRoot: String = "/users"

  "UserController" should {

    "render new user page" in {
      val getFormRequest = FakeRequest(GET, userRoot + "/new")
      sendFakeRequestAndMapResult(getFormRequest,
        statusMustBe(OK),
        contentMustInclude("email"), contentMustInclude("name"), contentMustInclude("password"))
    }

    def usersDAO(implicit app: Application) = {
      val app2UsersDAO = Application.instanceCache[UsersDAO]
      app2UsersDAO(app)
    }
    //invalid data
    "should be able to create new account" in {
      forAll(properPasswordsUpTo(20), properEmails, properNames) { (password: String, email: String, name: String) =>
        usersDAO.deleteAll
        val newUserJson: JsValue = Json.parse(s"""{"name": "$name", "password": "$password", "email":"$email"}""")
        val createUserRequest = (properJson: JsValue) => FakeRequest(POST, userRoot).withJsonBody(properJson)
        sendFakeRequestAndMapResult(createUserRequest(newUserJson),
          statusMustBe(SEE_OTHER), redirectLocationMustBeSome("/"), flashMustBeSome("success", "You have created account"))

        usersDAO.findByEmail(email).map { createdUser =>
          createdUser mustNot be(None)
          createdUser.map(_.email mustBe email)
          createdUser.map(u => u.name mustBe (if (name.isEmpty) None else Some(name)))
        }
      }

    }
    //TODO: invalid data
    "should be able to log in" in {
      forAll(properEmails, properPasswordsUpTo(20)) { (email: String, password: String) =>
        val newUser: User = User(None, email, password, None)
        usersDAO.insert(newUser)
        val newUserLoginJson: JsValue = Json.parse(s"""{"email":"$email", "password":"$password"}""")
        sendFakeRequestAndMapResult(loginRequest(newUserLoginJson),
          statusMustBe(SEE_OTHER), redirectLocationMustBeSome("/"), flashMustBeSome("success", "You have logged in!"),
          sessionMustContainKV("userEmail", email), sessionMustContainKV("isLogged", "true"))

      }

    }

    "should be able to update data when logged in" in {
      forAll(properPasswordsUpTo(17), properEmails, properNames) { (password: String, email: String, name: String) =>
        {
          val newUser: User = User(None, email, password, None)
          val userId = Await.result(usersDAO.insert(newUser), Duration.Inf)
          val updateUserJson: JsValue = Json.parse(s"""{"id": "$userId", "email":"new$email", "password":"new$password", "name":"$name"}""")
          val fakeRequestWithSession = FakeRequest(PUT, userRoot + s"/$userId")
            .withJsonBody(updateUserJson)
            .withSession("userEmail" -> email, "isLogged" -> "true")

          sendFakeRequestAndMapResult(fakeRequestWithSession,
            statusMustBe(SEE_OTHER), redirectLocationMustBeSome("/"), flashMustBeSome("success", "your profile has beed updated"))

          usersDAO.findById(userId).map(
            _.map(u => {
              u.id mustBe Some(userId)
              u.email mustBe "new" + email
              u.name mustBe (if (name.isEmpty) None else Some(name))
              u.password mustBe "new" + password
            }))

        }
      }
    }

    "should be able to log out" in {
      val email = "email@email.com"
      val password = "Test!234"
      val newUser: User = User(None, email, password, None)
      usersDAO.insert(newUser)
      val newUserLoginJson: JsValue = Json.parse(s"""{"email":"$email", "password":"$password"}""")
      sendFakeRequestAndMapResult(loginRequest(newUserLoginJson), sessionMustContainKV("userEmail", email))
      val logoutRequest = FakeRequest(POST, userRoot + "/logout")
      val sessionMustBeEmpty = (f: Future[Result]) => session(f) mustBe empty
      sendFakeRequestAndMapResult(logoutRequest, sessionMustBeEmpty)
    }

    "should be able to delete his account" in {

      val email = "email@email.com"
      val password = "Test!234"
      val newUser: User = User(None, email, password, None)
      val statusMustBeSeeOther = (f: Future[Result]) => status(f) mustBe SEE_OTHER
      usersDAO.insert(newUser)
      val deleteRequest = FakeRequest(DELETE, userRoot).withSession("userEmail" -> email, "isLogged" -> "true")
      sendFakeRequestAndMapResult(deleteRequest, statusMustBeSeeOther)

      usersDAO.findByEmail(email).map(_ mustBe None)
    }

  }

  val loginRequest = (userLoginJson: JsValue) => FakeRequest(POST, userRoot + "/login").withJsonBody(userLoginJson)
  val sessionMustContainKV = (key: String, value: String) => (f: Future[Result]) => session(f).get(key) mustBe Some(value)
  val statusMustBe = (status: Int) => (f: Future[Result]) => Await.result(f, Duration.Inf).header.status mustBe status
  val redirectLocationMustBeSome = (location: String) => (f: Future[Result]) => redirectLocation(f) mustBe Some(location)
  val flashMustBeSome = (flashName: String, flashContent: String) => (f: Future[Result]) => flash(f).get(flashName) mustBe Some(flashContent)
  val contentMustInclude = (includee: String) => (f: Future[Result]) => contentAsString(f) must include(includee)
  
  def sendFakeRequestAndMapResult[T](req: FakeRequest[T], f: (Future[Result] => Unit)*)(implicit w: Writeable[T]) = {
    route(app, req).map(result => f.foreach(_(result)))
  }

  private val specialChars: Seq[Char] = Array('!', '@', '#', '$', '%', '^', '&', '*', '(', ')')
  def generateUpToElements[A] = (gen: Gen[A], upperBound: Int) =>
    Gen.oneOf(1 to upperBound).flatMap { n =>
      Gen.listOfN(n, gen).flatMap { e => (e, n) }
    }

  def createGeneratorFromSequenceAndSize[A](generators: Seq[Gen[A]], min: Int, max: Int, f: (Gen[A], Int) => Gen[(List[A], Int)]): Gen[Seq[A]] = {
    val start = min - generators.size + 1
    val ending = max - generators.size + 1
    def recHelper(recGenerators: Seq[Gen[A]], accu: Seq[A], upperBound: Int, upperBoundSubtraction: Int): Gen[Seq[A]] =
      recGenerators match {
        case x :: xs if (x :: xs == generators) => Gen.choose(start, ending).flatMap(f(x, _))
          .flatMap { case (elements, n) => recHelper(xs, accu ++ elements, upperBound + 1, upperBoundSubtraction + n) }
        case x :: xs => f(x, upperBound - upperBoundSubtraction)
          .flatMap { case (elements, n) => recHelper(xs, accu ++ elements, upperBound + 1, upperBoundSubtraction + n) }
        case Nil => accu
      }
    recHelper(generators, Nil, ending, 0).suchThat(_.length >= min)
  }

  def properPasswordsUpTo(maxSize: Int): Gen[String] = {
    val generators: Seq[Gen[Char]] = List(Gen.alphaLowerChar, Gen.alphaUpperChar, Gen.numChar, Gen.oneOf(specialChars))
    createGeneratorFromSequenceAndSize(generators, 8, maxSize, generateUpToElements)
      .flatMap(Random.shuffle(_).mkString)
  }

  val properEmails: Gen[String] = Gen.alphaStr
    .suchThat(email => email.length > 0)
    .flatMap(_ + "@test.com")

  val properNames: Gen[String] = Gen.alphaStr
}