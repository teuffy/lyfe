package postings

import util.TestUtil._
import org.scalatestplus.play.PlaySpec
import org.scalatestplus.play.OneAppPerTest
import org.scalatest.prop.PropertyChecks
import play.api.test._
import play.api.test.Helpers._
import play.api.http.Writeable
import scala.concurrent.Future
import play.api.mvc.Result



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

  }
  def sendFakeRequestAndMapResult[T](req: FakeRequest[T], f: (Future[Result] => Unit)*)(implicit w: Writeable[T]) = {
    route(app, req).map(result => f.foreach(_(result)))
  }
}