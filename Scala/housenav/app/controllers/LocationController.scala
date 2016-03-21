package controllers

import javax.inject.Inject
import dao.LocationsDAO
import play.api.mvc.Controller
import play.api.data.Form
import play.api.data.Forms.{ date, longNumber, mapping, nonEmptyText, optional, of }
import play.api.data.format.Formats._
import models.Location
import play.api.mvc.Action
import scala.concurrent.Future
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.i18n.I18nSupport
import play.api.i18n.MessagesApi

class LocationController @Inject() (locationDAO: LocationsDAO, val messagesApi: MessagesApi) extends Controller with I18nSupport {
  
  val Home = Redirect(routes.LocationController.list)

  val locationForm: Form[Location] = Form(
    mapping(
      "id" -> optional(longNumber),
      "lat" -> of(doubleFormat),
      "long" -> of(doubleFormat))(Location.apply)(Location.unapply))
      
  def list = Action.async { implicit rs =>
    locationDAO.getAll.map (locations =>  Ok(views.html.locations(locations)))
  }
      
  def create = Action.async { implicit rs =>
      Future.successful(Ok(views.html.newLocationForm(locationForm)))    
  }
      
  def save = Action.async { implicit rs =>
    locationForm.bindFromRequest.fold(
        caseformWithErrors => Future.successful(BadRequest),
        location => {
          for {
            _ <- locationDAO.insert(location)
          } yield Home.flashing("success" -> "Location %s has been created".format(location.id))
        })
  }

}