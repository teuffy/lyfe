package controllers

import javax.inject.Inject
import javax.inject.Singleton
import play.api.mvc.Controller
import play.api.mvc.Action


@Singleton
class ApplicationController @Inject() extends Controller {
  def index = Action {
    Ok(views.html index("Your new application is COOL."))
  }

}