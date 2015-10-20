package controllers

import play.api.libs.json.Json
import play.api.mvc._
import models._
import utils.JsonUtils
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

/**
 * Employees controller
 */
object Employees extends Controller with JsonUtils {

  def getEmployees = Action.async { implicit request =>
    Employee.findAll() map { employees =>
      val parsedEmployees = employees.map(e =>
        addSelfLink(Json.toJson(EmployeeData.fromEmployee(e)), routes.Employees.getEmployee(e.id))
      )

      Ok(Json.obj("employees" -> parsedEmployees))
    }
  }

  def getEmployee(id: Int) = Action.async { implicit request =>
    Employee.find(id) map { employeeOpt =>
      employeeOpt map { employee =>
        val json = addSelfLink(Json.toJson(EmployeeData.fromEmployee(employee)), routes.Employees.getEmployee(id)) ++ Json.obj(
          "links" -> Seq(
            createLink("time-entries", routes.Employees.getTimeEntriesForEmployee(id)),
            createLink("projects", routes.Employees.getProjectsForEmployee(id))
          )
        )

        Ok(json)
      } getOrElse NotFound(errorJson("The requested employee could not be found"))
    }
  }

  def createEmployee = Action.async(parse.json) { implicit request =>
    request.body.validate[EmployeeData].fold(
      errors => Future.successful(BadRequest(errorJson(errors))),
      employeeData => {
        Employee.create(employeeData) map { e =>
          Created.withHeaders(LOCATION -> routes.Employees.getEmployee(e.id).absoluteURL())
        }
      }
    )
  }

  def editEmployee(id: Int) = Action.async(parse.json) { implicit request =>
    request.body.validate[EmployeeData].fold(
      errors => Future.successful(BadRequest(errorJson(errors))),
      employeeData => {
        Employee.find(id) map { employeeOpt =>
          employeeOpt map {employee =>
            Employee.save(employee)
            NoContent
          } getOrElse NotFound(errorJson("The requested employee could not be found"))
        }
      }
    )
  }

  def deleteEmployee(id: Int) = Action.async(parse.empty) { implicit request =>
    Employee.find(id) map { employeeOpt =>
      employeeOpt map { employee =>
        // Delete all linked TimeEntries (do this normally with a query)
        TimeEntry.findByEmployee(id).map(_.foreach(TimeEntry.destroy))

        Employee.destroy(employee)

        NoContent
      } getOrElse NotFound(errorJson("The requested employee could not be found"))
    }
  }

  def searchEmployees(firstNameOpt: Option[String], jobTitleOpt: Option[String]) = Action.async { implicit request =>
    Employee.findByNameAndJobTitle(firstNameOpt, jobTitleOpt) fold (
      errorMessage => Future.successful(BadRequest(errorJson(errorMessage))),
      futureEmployee => futureEmployee map { employees =>

        val json = employees.map(e =>
          addSelfLink(Json.toJson(EmployeeData.fromEmployee(e)), routes.Employees.getEmployee(e.id))
        )

        Ok(Json.obj("employees" -> json))
      }
    )
  }

  def getProjectsForEmployee(id: Int) = Action.async { implicit request =>
    Employee.find(id) flatMap { employeeOpt =>
      employeeOpt map { employee =>
        Project.findByEmployee(employee.id) map { projects =>
          val parsedProjects = projects map { p =>
            addSelfLink(Json.toJson(ProjectData.fromProject(p)), routes.Projects.getProject(p.id))
          }

          Ok(Json.obj("projects" -> parsedProjects))
        }
      } getOrElse Future.successful(NotFound(errorJson("The requested employee could not be found")))
    }
  }

  def getTimeEntriesForEmployee(id: Int) = Action.async { implicit request =>
    Employee.find(id) flatMap { employeeOpt =>
      employeeOpt map { employee =>
        TimeEntry.findByEmployee(employee.id) map { timeEntries =>
          val parsedTimeEntries = timeEntries map { te =>
            addSelfLink(Json.toJson(TimeEntryData.fromTimeEntry(te)), routes.TimeEntries.getTimeEntry(te.id))
          }

          Ok(Json.obj("timeEntries" -> parsedTimeEntries))
        }
      } getOrElse Future.successful(NotFound(errorJson("The requested employee could not be found")))
    }
  }

}
