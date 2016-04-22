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
import dao.UsersDAO
import models.User

@Singleton
class PostingController @Inject() (advertisementsDAO: AdvertisementsDAO, utils: ControllerUtils, val messagesApi: MessagesApi) extends Controller with I18nSupport with Secured {
  lazy val postingForm: Form[Advertisement] = Form(
    mapping(
      "id" -> optional(longNumber),
      "address" -> text,
      "adType" -> text,
      "price" -> of(doubleFormat),
      "pricePeriod" -> text,
      "noOfRooms" -> number,
      "sellerType" -> text,
      "size" -> number,
      "userId" -> optional(longNumber))(AdvertisementExtra.applyFromForm)(AdvertisementExtra.unapplyFromForm))

  def createNewPostingForm = Action {
    Ok(views.html.newPostingForm(postingForm))
  }

  def savePosting = IsAuthenticated { implicit userEmail =>
    implicit request =>
      postingForm.bindFromRequest.fold(
        formWithErrors => {
          Future(BadRequest(views.html.newPostingForm(formWithErrors)))
        },
        advertisement => {
          utils.performWithUser {
            (user: User) =>
              advertisementsDAO.insertForUser(advertisement, user)
              Future(Redirect(routes.ApplicationController.index).flashing("success" -> "You have created posting"))
          }
        })
  }
}