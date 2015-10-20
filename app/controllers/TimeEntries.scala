package controllers

import models._
import org.joda.time.DateTime
import play.api.libs.json.Json
import play.api.mvc._
import utils.JsonUtils
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

/**
 * TimeEntries controller
 */
object TimeEntries extends Controller with JsonUtils {

  def getTimeEntries = Action.async { implicit request =>
    TimeEntry.findAll() map { timeEntries =>
      val parsedTimeEntries = timeEntries.map { te =>
        addSelfLink(Json.toJson(TimeEntryData.fromTimeEntry(te)), routes.TimeEntries.getTimeEntry(te.id))
      }

      Ok(Json.obj("timeEntries" -> parsedTimeEntries))
    }
  }

  def getTimeEntry(id: Int) = Action.async { implicit request =>
    TimeEntry.find(id) map { timeEntryOpt =>
      timeEntryOpt map { te =>
        val json = addSelfLink(Json.toJson(TimeEntryData.fromTimeEntry(te)), routes.TimeEntries.getTimeEntry(te.id)) ++ Json.obj("links" -> Seq(
          createLink("employee", routes.Employees.getEmployee(te.employeeId)),
          createLink("project", routes.Projects.getProject(te.projectId))
        ))

        Ok(json)
      } getOrElse NotFound(errorJson("The requested time entry could not be found"))
    }
  }

  def createTimeEntry = Action.async(parse.json) { implicit request =>
    request.body.validate[TimeEntryData].fold(
      errors => Future.successful(BadRequest(errorJson(errors))),
      timeEntryData =>
        TimeEntry.create(timeEntryData) map { timeEntry =>
          // Add link to the location header of the newly created TimeEntry
          Created.withHeaders(LOCATION -> routes.TimeEntries.getTimeEntry(timeEntry.id).absoluteURL())

        }
    )
  }

  def editTimeEntry(id: Int) = Action.async(parse.json) { implicit request =>
    request.body.validate[TimeEntryData].fold(
      errors => Future.successful(BadRequest(errorJson(errors))),
      timeEntryData =>
        TimeEntry.find(id) map { timeEntryOpt =>
          timeEntryOpt map { timeEntry =>
            TimeEntry.save(timeEntry)
            NoContent
          } getOrElse NotFound(errorJson("The request time entry could not be found"))
        }
    )
  }

  def deleteTimeEntry(id: Int) = Action.async(parse.empty) { implicit request =>
    TimeEntry.find(id) map { timeEntryOpt =>
      timeEntryOpt map { timeEntry =>
        TimeEntry.destroy(timeEntry)

        NoContent
      } getOrElse NotFound(errorJson("The request time entry could not be found"))
    }
  }

  def searchTimeEntries(from: Option[DateTime], to: Option[DateTime]) = Action.async { implicit request =>
    TimeEntry.findFromAndTo(from, to).fold(
        error => Future.successful(BadRequest(errorJson(error))),
        timeEntriesFuture => timeEntriesFuture map { timeEntries =>

          val json = timeEntries.map{te =>
            addSelfLink(Json.toJson(TimeEntryData.fromTimeEntry(te)),routes.TimeEntries.getTimeEntry(te.id))
          }

          Ok(Json.obj("timeEntries" -> json))
        }
      )
  }
}
