package controllers

import javax.inject.{ Singleton, Inject}
import play.api.i18n.I18nSupport
import play.api.mvc.Controller
import models.Advertisement
import play.api.data._
import play.api.data.Forms._
import models.AdType
import play.api.data.format.Formats._
import play.api.mvc.Action
import models.AdvertisementExtra
import play.api.i18n.MessagesApi
import play.api.mvc.AnyContent

@Singleton
class PostingController @Inject() (val messagesApi: MessagesApi) extends Controller with I18nSupport with Secured {
  //case class Advertisement(id: Option[Long], Address: String, adType: AdType, price: Double, pricePeriod: PricePeriod, noOfRooms: Int, sellerType: SellerType, size: Int)
  lazy val postingForm: Form[Advertisement] = Form(
      mapping(
          "id" -> optional(longNumber),
          "address" -> text,
          "adType" -> text,
          "price" -> of(doubleFormat),
          "pricePeriod" -> text,
          "noOfRooms" -> number,
          "sellerType" -> text,
          "size" -> number
      )(AdvertisementExtra.applyFromForm)(AdvertisementExtra.unapplyFromForm))
      
  def createNewPostingForm = Action {
    Ok(views.html.newPostingForm(postingForm))
  }
  
  def savePosting: Action[AnyContent] = ???
}