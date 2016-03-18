
import org.scalatestplus.play._
import play.api.test._
import play.api.test.Helpers._
import org.scalacheck.Properties
import org.scalacheck.Prop.forAll

class ApplicationSpec extends Properties("String") with PlaySpec with OneAppPerTest {

  "Routes" should {
    "send 404 on a bad request" in {
      val NonExistingEndpoint: String = "/iamjustanonexistingtest"
      route(app, FakeRequest(GET, NonExistingEndpoint)).map(status(_)) mustBe Some(NOT_FOUND)
    }
  }

  "ApplicationController" should {

    "render the index page" in {
      val home = route(app, FakeRequest(GET, "/")) get

      status(home) mustBe OK
      contentType(home) mustBe Some("text/html")
      contentAsString(home) must include("Your new application is COOL.")
    }
  }

  "ParamController" should {
    val emptyMessage = (msg: String) => route(app, FakeRequest(GET, "/test/" ++ msg)) get

    "return passed string message" in {
      property("startsWith") = forAll { (msg: String) =>
        {
          println(msg)
          val message = emptyMessage(msg)
          contentAsString(message) contains (msg + "dupa")

        }
      }
    }
  }

}