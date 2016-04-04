package controllers

import play.api.mvc._
import scala.concurrent.Future

trait Secured {

  private def userInfo(request: RequestHeader): Option[String] = request.session.get("userEmail")

  private def onUnauthorized(request: RequestHeader) =
    Results.Redirect(routes.UserController.createLoginForm())

  def IsAuthenticated(f: => String => Request[AnyContent] => Future[Result]) =
    Security.Authenticated(userInfo, onUnauthorized) { userData =>
      Action.async { request => f(userData)(request) }
    }

}