package controllers

import models.User
import dao.UsersDAO
import javax.inject.Inject
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.mvc.Results.Forbidden
import scala.concurrent.Future

class ControllerUtils @Inject() (usersDAO: UsersDAO) {
  def performWithUser[S](f: User => Future[play.api.mvc.Result])(implicit userEmail: String) = usersDAO.findByEmail(userEmail).flatMap {
    case None => Future(Forbidden)
    case Some(user) => f(user)
  }
}
