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

        "should be able to create new user" in {
            implicit val generatorDrivenConfig =
                PropertyCheckConfig(maxSize = 20)

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
                            .flatMap { case (chars, n) => recHelper(xs, accu ++ chars, upperBound + 1, upperBoundSubtraction + n) }
                        case x :: xs => f(x, upperBound - upperBoundSubtraction).flatMap { case (chars, n) => recHelper(xs, accu ++ chars, upperBound + 1, upperBoundSubtraction + n) }
                        case Nil => accu

                    }
                recHelper(generators, Nil, ending, 0)
            }

            val properPasswords: Gen[String] = {
                val generators: Seq[Gen[Char]] = List(Gen.alphaLowerChar, Gen.alphaUpperChar, Gen.numChar, Gen.oneOf(specialChars))
                createGeneratorFromSequenceAndSize(generators, 8, 20, generateUpToElements)
                    .flatMap(Random.shuffle(_).mkString)
            }

            val properEmails: Gen[String] = Gen.alphaStr
                .suchThat(email => email.length > 0)
                .flatMap(_ + "@test.com")
            val properNames: Gen[String] = Gen.alphaStr
                .suchThat(e => e.isEmpty || e.length > 2)
            forAll(properPasswords, properEmails, properNames) { (password: String, email: String, name: String) =>
                usersDAO.deleteAll
                val properJson: JsValue = Json.parse(s"""{"name": "$name", "password": "$password", "email":"$email"}""")
                val createUser: Future[Result] = route(app, FakeRequest(POST, userRoot + "/new").withJsonBody(properJson)) get

                status(createUser) mustBe SEE_OTHER
                redirectLocation(createUser) mustBe Some("/")
                flash(createUser).get("success") mustBe Some("You have created account")

                val getFirstUserFromSeq = (seq: Seq[User]) =>
                    seq.head

                val createdUser: User = Await.result(usersDAO.getAll.map(getFirstUserFromSeq), Duration.Inf)
                createdUser.email mustBe email
                createdUser.name mustBe (if (name.isEmpty) None else Some(name))
            }

        }

//        "should validate fields" in {
//            fail
//        }
//
//        "should be able to log in" in {
//            fail
//        }
//
//        "should be able to update data when logged in" in {
//            fail
//        }
//
//        "should be able to log out" in {
//            fail
//        }
//
//        "should be able to delete his account" in {
//            fail
//        }

    }

}