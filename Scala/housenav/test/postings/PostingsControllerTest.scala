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
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.mvc.Result
import play.api.test._
import play.api.test.Helpers._
import util.TestUtil._
import org.scalacheck.Gen
import models.AdType
import models.PricePeriod
import models.SellerType
import models.PricePeriod._
import models.SellerType._
import models.AdType._
import models.Advertisement

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
    
      val email = "test@test.com"
      val password = "Test!234"
      val newUser: User = User(Some(1), email, password, None)
      usersDAO.insert(newUser)


    def advertisementsDAO(implicit app: Application) = {
      val app2AdvertisementsDAO = Application.instanceCache[AdvertisementsDAO]
      app2AdvertisementsDAO(app)
    }
    
    "when logged in, should be able to add posting" in {

      forAll(Gen.numStr.suchThat(_.length > 0), enumGen(AdType), Gen.posNum[Double], enumGen(PricePeriod), Gen.posNum[Int], enumGen(SellerType)) {
        (address: String, adType: AdType, price: Double, pricePeriod: PricePeriod, n: Int, sellerType: SellerType) =>
          {
            val newPostingRequest: JsValue = Json.parse(
              s"""{"address":"$address", "adType":"$adType", "price": $price, "pricePeriod": "$pricePeriod", "noOfRooms": $n, "sellerType": "$sellerType", "size": $n}"""")
            val fakeRequestWithSession = FakeRequest(POST, postingsRoot)
              .withJsonBody(newPostingRequest)
              .withSession("userEmail" -> email, "isLogged" -> "true")
            sendFakeRequestAndMapResult(fakeRequestWithSession,
              statusMustBe(SEE_OTHER), redirectLocationMustBeSome("/"), flashMustBeSome("success", "You have created posting"))
          }
      }
    }

    "when logged in, should be able to update posting" in {
      val email = "test@test.com"
      val password = "Test!234"
      val newUser: User = User(None, email, password, None)
      usersDAO.insert(newUser)
      val existingPosting = Advertisement(None, "Testable address", AdType.Flat, 20.0, PricePeriod.Daily, 1, SellerType.Agency, 1, newUser.id)
      advertisementsDAO.insert(existingPosting)
      //TODO: acutal test
      
    }

  }

}