package controllers

import play.api.cache.Cached
import play.api.libs.json.Json
import play.api.mvc._
import scala.concurrent.Future
import play.api.Play.current

object Application extends Controller {

  def index = Cached.status(_ => "apiIndex", OK, 5000) {
    Action.async { implicit request =>

      val json = Json.obj(
        "version" -> "0.2",
        "links" -> Seq(
          Json.obj("rel" -> "projects", "href" -> routes.Projects.getProjects().absoluteURL()),
          Json.obj("rel" -> "employees", "href" -> routes.Employees.getEmployees().absoluteURL()),
          Json.obj("rel" -> "time-entries","href" -> routes.TimeEntries.getTimeEntries().absoluteURL())
        )
      )

      Future.successful(Ok(json))
    }
  }

}