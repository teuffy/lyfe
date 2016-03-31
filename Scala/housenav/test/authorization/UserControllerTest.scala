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
            val alphaNumerics = ('!' to ')') ++ ('a' to 'z') ++ ('0' to '9') ++ ('A' to 'Z')
            val gene = Gen.listOf(alphaNumerics)
            
            val properPasswords: Gen[String] = 
                (Gen.choose(6, 20) flatMap {n => Gen.listOfN(n, alphaNumerics).flatMap { e =>  e.mkString}})
            val properEmails: Gen[String] = arbitrary[String]
                .filter(_.length > 0)
                .flatMap(_ + "@test.com")
            val properNames: Gen[String] = arbitrary[String]
                .filter(e => e.isEmpty || e.length > 2 && e.length < 21)
            forAll(properPasswords, properEmails, properNames) { (password: String, email: String, name: String) =>
                val properJson: JsValue = Json.parse(s"""{"name": "$name", "password": "$password", "email":"$email"}""")
                val createUser: Future[Result] = route(app, FakeRequest(POST, userRoot + "/new").withJsonBody(properJson)) get

                status(createUser) mustBe SEE_OTHER
                redirectLocation(createUser) mustBe Some("/")
                flash(createUser).get("success") mustBe Some("You have created account")

                val getFirstUserByEmailFromSeq = (seq: Seq[User]) =>
                    seq.filter(_.email == email).head

                val createdUser: User = Await.result(usersDAO.getAll.map(getFirstUserByEmailFromSeq), Duration.Inf)

                createdUser.email mustBe email
                createdUser.name mustBe (if (name.isEmpty) None else Some(name))
            }

        }

        "should validate fields" in {
            fail
        }

        "should be able to log in" in {
            fail
        }

        "should be able to update data when logged in" in {
            fail
        }

        "should be able to log out" in {
            fail
        }

        "should be able to delete his account" in {
            fail
        }

    }

}