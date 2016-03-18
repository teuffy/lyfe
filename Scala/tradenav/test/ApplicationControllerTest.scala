
import org.scalatestplus.play._
import play.api.test._
import play.api.test.Helpers._
import org.scalatest.prop.PropertyChecks

class ApplicationSpec extends PlaySpec with PropertyChecks with OneAppPerTest {

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
      forAll { (msg: String) =>
        {
          println(msg)
          val message = emptyMessage(msg)
          contentAsString(message) contains (msg + "dupa")

        }
      }
    }
  }

}