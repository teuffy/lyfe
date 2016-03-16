package controllers

import play.api.mvc.Controller

object Application extends Controller {
  Ok(views.html.index("Your new application is ready!"))
}