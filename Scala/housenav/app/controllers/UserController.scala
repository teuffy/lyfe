package controllers

import javax.inject.Inject
import javax.inject.Singleton
import dao.LocationsDAO
import play.api.mvc.Controller
import play.api.data.Form
import play.api.data.Forms._
import play.api.data.format.Formats._
import models.Location
import play.api.mvc.Action
import scala.concurrent.Future
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.i18n.I18nSupport
import play.api.i18n.MessagesApi
import models.User
import play.api.mvc.AnyContent
import play.mvc.BodyParser.Text
import dao.UsersDAO

@Singleton
class UserController @Inject() (usersDAO: UsersDAO, val messagesApi: MessagesApi) extends Controller with I18nSupport {

    lazy val newUserForm: Form[User] = Form(
        mapping(
            "id" -> optional(longNumber),
            "email" -> email,
            "password" -> nonEmptyText(8, 20).verifying(isAlphaNumericString),
            "name" -> optional(text))(User.apply)(User.unapply))
    lazy val loginForm = Form(
        tuple(
            "email" -> text,
            "password" -> text) verifying ("Invalid user or password", result => result match {
                case (email, password) => {
                    usersDAO.authenticate(email, password)
                    true
                }
                case _ => false
            }))
    private def isAlphaNumericString: String => Boolean =
        (s: String) => s.matches("""(?=.*[a-z])(?=.*[A-Z])(?=.*\d)(?=.*(_|[^\w])).+""")

    def createNewUserForm: Action[AnyContent] = Action {
        Ok(views.html.newUserForm(newUserForm))
    }
        
    def login: Action[AnyContent] = Action { implicit request =>
        loginForm.bindFromRequest.fold(
                formWithErrors => BadRequest(views.html.loginForm(formWithErrors)),
                logged => Redirect(routes.ApplicationController.index).flashing("success" -> "You have logged in").withSession("userEmail" -> logged._1, "logged" -> "true")
                
                )
    }

    def saveUser: Action[AnyContent] = Action { implicit request =>
        newUserForm.bindFromRequest.fold(
            formWithErrors => {
                BadRequest(views.html.newUserForm(formWithErrors))
            },
            user => {
                usersDAO.insert(user)
                Redirect(routes.ApplicationController.index).flashing("success" -> "You have created account")
            })
    }

    

}