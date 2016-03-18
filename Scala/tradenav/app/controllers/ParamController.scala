package controllers

import javax.inject.Inject
import javax.inject.Singleton
import play.api.mvc.Controller
import play.api.mvc.Action

@Singleton
class ParamController @Inject() extends Controller {
  def msg(message: String) = Action {
    Ok(views.html.index(message))
    
  }
  
}