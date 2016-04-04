package authorization

import org.scalatest.prop.PropertyChecks
import org.scalatestplus.play.OneAppPerTest
import org.scalatestplus.play.PlaySpec
import play.api.test._
import play.api.test.Helpers._
import play.api.libs.json.Json
import javax.inject.Inject
import play.api.libs.concurrent.Execution.Implicits._
import play.api.Application
import scala.concurrent.Future
import play.api.mvc.Result
import play.api.libs.json.JsValue
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import models.User
import org.scalacheck.Gen
import dao.UsersDAO
import org.scalacheck.Arbitrary._
import scala.util.Random
import org.scalatest.BeforeAndAfterEach

class UserControllerTest extends PlaySpec with PropertyChecks with OneAppPerTest {

  "UserController" should {

    val userRoot: String = "/user"

    "render new user page" in {
      val userHome: Future[Result] = route(app, FakeRequest(GET, userRoot + "/new")) get

      status(userHome) mustBe OK
      contentType(userHome) mustBe Some("text/html")
      contentAsString(userHome) must include("email")
      contentAsString(userHome) must include("name")
      contentAsString(userHome) must include("password")
    }

    def usersDAO(implicit app: Application) = {
      val app2UsersDAO = Application.instanceCache[UsersDAO]
      app2UsersDAO(app)
    }
    //invalid data
    "should be able to create new account" in {
      forAll(properPasswordsUpTo(20), properEmails, properNames) { (password: String, email: String, name: String) =>
        {
          usersDAO.deleteAll
          val properJson: JsValue = Json.parse(s"""{"name": "$name", "password": "$password", "email":"$email"}""")
          val createUserFuture: Future[Result] = route(app, FakeRequest(POST, userRoot + "/new").withJsonBody(properJson)) get
          val createUser = Await.result(createUserFuture, Duration.Inf)

          status(createUserFuture) mustBe SEE_OTHER
          redirectLocation(createUserFuture) mustBe Some("/")
          flash(createUserFuture).get("success") mustBe Some("You have created account")

          val getFirstUserFromSeq = (seq: Seq[User]) =>
            seq.headOption

          val createdUser: Option[User] = Await.result(usersDAO.getAll.map(getFirstUserFromSeq), Duration.Inf)
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
        val loginAsUser: Future[Result] = route(app, FakeRequest(POST, userRoot + "/login").withJsonBody(newUserLoginJson)) get

        status(loginAsUser) mustBe SEE_OTHER
        redirectLocation(loginAsUser) mustBe Some("/")
        flash(loginAsUser).get("success") mustBe Some("You have logged in!")
        session(loginAsUser).get("userEmail") mustBe Some(email)
        session(loginAsUser).get("isLogged") mustBe Some("true")
      }

    }

    "should be able to update data when logged in" in {
      forAll(properPasswordsUpTo(17), properEmails, properNames) { (password: String, email: String, name: String) =>
        {
          val newUser: User = User(None, email, password, None)
          val userId = Await.result(usersDAO.insert(newUser), Duration.Inf)
          val updateUserJson: JsValue = Json.parse(s"""{"id": "$userId", "email":"new$email", "password":"new$password", "name":"$name"}""")
          val fakeRequestWithSession = FakeRequest(PUT, userRoot + s"/$userId").withJsonBody(updateUserJson).withSession("userEmail" -> email, "isLogged" -> "true")
          val updateUserFuture = route(app, fakeRequestWithSession) get
          val updateUser = Await.result(updateUserFuture, Duration.Inf)

          status(updateUserFuture) mustBe SEE_OTHER
          redirectLocation(updateUserFuture) mustBe Some("/")
          flash(updateUserFuture).get("success") mustBe Some("your profile has beed updated")

          val updatedUser: Option[User] = Await.result(usersDAO.findById(userId), Duration.Inf)
          updatedUser mustNot be(None)
          updatedUser.map(u => {
            u.id mustBe Some(userId)
            u.email mustBe "new" + email
            u.name mustBe (if (name.isEmpty) None else Some(name))
            u.password mustBe "new" + password
          })
        }
      }
    }

    "should be able to log out" in {
      val email = "email@email.com"
      val password = "Test!234"
      val newUser: User = User(None, email, password, None)
      val userId = Await.result(usersDAO.insert(newUser), Duration.Inf)
      val newUserLoginJson: JsValue = Json.parse(s"""{"email":"$email", "password":"$password"}""")
      val loginAsUserFuture: Future[Result] = route(app, FakeRequest(POST, userRoot + "/login").withJsonBody(newUserLoginJson)) get

      session(loginAsUserFuture).get("userEmail") mustBe Some(email)
      val logout = route(app, FakeRequest(POST, userRoot + "/logout")) get
      
      session(logout) mustBe empty
    }

    "should be able to delete his account" in {
      val email = "email@email.com"
      val password = "Test!234"
      val newUser: User = User(None, email, password, None)
      val userId = Await.result(usersDAO.insert(newUser), Duration.Inf)
      val deleteFuture = route(app, FakeRequest(DELETE, userRoot + s"/$userId").withSession("userEmail" -> email, "isLogged" -> "true")).get

      status(deleteFuture) mustBe SEE_OTHER
      val deletedUser: Option[User] = Await.result(usersDAO.findById(userId), Duration.Inf)
      deletedUser must be(None)
    }

  }
  val specialChars: Seq[Char] = Array('!', '@', '#', '$', '%', '^', '&', '*', '(', ')')
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