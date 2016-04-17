package postings

import scala.concurrent.Future

import org.scalatest.prop.PropertyChecks
import org.scalatestplus.play.{ OneAppPerTest, PlaySpec }

import dao.{ AdvertisementsDAO, UsersDAO }
import javax.inject.Singleton
import models.User
import play.api.Application
import play.api.http.Writeable
import play.api.libs.json.{ JsValue, Json }
import play.api.mvc.Result
import play.api.test._
import play.api.test.Helpers._
import util.TestUtil._

class PostingsControllerTest extends PlaySpec with PropertyChecks with OneAppPerTest {

  val postingsRoot: String = "/postings"

  "Postings controller" should {

    "render new posting page" in {
      val getFormRequest = FakeRequest(GET, postingsRoot + "/new")
      sendFakeRequestAndMapResult(getFormRequest,
        statusMustBe(OK),
        contentMustInclude("address"), contentMustInclude("property type"), contentMustInclude("price"),
        contentMustInclude("Rent paid per"), contentMustInclude("Number of rooms"), contentMustInclude("direct or via agency"),
        contentMustInclude("size"))
    }

    def usersDAO(implicit app: Application) = {
      val app2UsersDAO = Application.instanceCache[UsersDAO]
      app2UsersDAO(app)
    }

    def advertisementsDAO(implicit app: Application) = {
      val app2AdvertisementsDAO = Application.instanceCache[AdvertisementsDAO]
      app2AdvertisementsDAO(app)
    }

    "when logged in, should be able to add posting" in {
      val email = "test@test.com"
      val password = "Test!234"
      val newUser: User = User(None, email, password, None)
      usersDAO.insert(newUser)
      val newUserLoginJson: JsValue = Json.parse(s"""{"email":"$email", "password":"$password"}""")
      sendFakeRequestAndMapResult(loginRequest(newUserLoginJson), sessionMustContainKV("userEmail", email))
      val newPostingRequest: JsValue = Json.parse("""{"address":"sample address", "adType":"flat", "price": 1245.66, "pricePeriod": "daily", "noOfRooms": 1, "sellerType": "direct", "size": 64}""")
      val fakeRequestWithSession = FakeRequest(POST, postingsRoot)
        .withJsonBody(newPostingRequest)
        .withSession("userEmail" -> email, "isLogged" -> "true")
      sendFakeRequestAndMapResult(fakeRequestWithSession, 
          statusMustBe(SEE_OTHER), redirectLocationMustBeSome("/"), flashMustBeSome("success", "You have created posting"))
    }

  }
  def sendFakeRequestAndMapResult[T](req: FakeRequest[T], f: (Future[Result] => Unit)*)(implicit w: Writeable[T]) = {
    route(app, req).map(result => f.foreach(_(result)))
  }
}