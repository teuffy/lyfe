package controllers

import scala.concurrent.Future

import dao.AdvertisementsDAO
import javax.inject.{ Inject, Singleton }
import models.{ Advertisement, AdvertisementExtra }
import play.api.data.Form
import play.api.data.Forms.{ longNumber, mapping, number, of, optional, text }
import play.api.data.format.Formats.doubleFormat
import play.api.i18n.{ I18nSupport, MessagesApi }
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.mvc.{ Action, AnyContent, Controller }

@Singleton
class PostingController @Inject() (advertisementsDAO: AdvertisementsDAO, val messagesApi: MessagesApi) extends Controller with I18nSupport with Secured {
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
  
  def savePosting: Action[AnyContent] = Action.async { implicit request =>
    postingForm.bindFromRequest.fold(
      formWithErrors => {
        Future(BadRequest(views.html.newPostingForm(formWithErrors)))
      },
      advertisement => {
        advertisementsDAO.insert(advertisement)
        Future(Redirect(routes.ApplicationController.index).flashing("success" -> "You have created posting"))
      })
  }
}