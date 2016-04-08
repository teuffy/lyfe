package controllers

import scala.concurrent.Future

import UserController.isAlphaNumericString
import dao.UsersDAO
import javax.inject.{ Inject, Singleton }
import models.User
import play.api.data.Form
import play.api.data.Forms.{ email, longNumber, mapping, nonEmptyText, optional, text, tuple }
import play.api.i18n.{ I18nSupport, MessagesApi }
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.mvc.{ Action, AnyContent, Controller }

@Singleton
class UserController @Inject() (usersDAO: UsersDAO, val messagesApi: MessagesApi) extends Controller with I18nSupport with Secured {
  lazy val userForm: Form[User] = Form(
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

  def createNewUserForm: Action[AnyContent] = Action {
    Ok(views.html.newUserForm(userForm))
  }

  def saveUser: Action[AnyContent] = Action.async { implicit request =>
    userForm.bindFromRequest.fold(
      formWithErrors => {
        Future(BadRequest(views.html.newUserForm(formWithErrors)))
      },
      user => {
        usersDAO.insert(user)
        Future(Redirect(routes.ApplicationController.index).flashing("success" -> "You have created account"))
      })
  }

  def createLoginForm: Action[AnyContent] = Action {
    Ok(views.html.loginForm(loginForm))
  }

  def login: Action[AnyContent] = Action.async { implicit request =>
    loginForm.bindFromRequest.fold(
      formWithErrors => Future(BadRequest(views.html.loginForm(formWithErrors))),
      logged => Future(Redirect(routes.ApplicationController.index)
        .flashing("success" -> "You have logged in!")
        .withSession("userEmail" -> logged._1, "isLogged" -> "true")))
  }

  def editProfile = IsAuthenticated { userData =>
    _ => usersDAO.findByEmail(userData).map {
        case Some(u) => Ok(views.html.editProfile(u.id.get, userForm.fill(u)))
        case _ => Forbidden
      }
  }

  def updateProfile(id: Long) =
    IsAuthenticated { userData =>
      implicit request => userForm.bindFromRequest.fold(
        formWithErrors => Future(BadRequest(views.html.editProfile(id, formWithErrors))),
        formUser => {
          usersDAO.update(id, formUser).map {
            case 1 => Redirect(routes.ApplicationController.index).flashing("success" -> "your profile has beed updated")
            case _ => Forbidden
          }
        })
    }

  def logout = Action {
    Redirect(routes.ApplicationController.index).withNewSession.flashing("success" -> "you have logged out")
  }

  def delete =
    IsAuthenticated { userData =>
      _ => usersDAO.findByEmail(userData).flatMap {
        case Some(u) => usersDAO.delete(u.id.get).map {
          case 1 =>
            Redirect(routes.ApplicationController.index)
              .flashing("success" -> "you have deleted your account")
              .withNewSession
          case _ => BadRequest
        }
        case _ => Future(Forbidden)
      }}

  
}

object UserController {
  private val isAlphaNumericString: String => Boolean =
    (s: String) => s.matches("""(?=.*[a-z])(?=.*[A-Z])(?=.*\d)(?=.*(_|[^\w])).+""")
 
}

