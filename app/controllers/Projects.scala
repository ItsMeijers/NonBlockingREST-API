package controllers

import models.{TimeEntryData, TimeEntry, ProjectData, Project}
import play.api.libs.json.Json
import play.api.mvc._
import scalikejdbc._
import utils.JsonUtils
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

/**
 * Projects Controller
 */
object Projects extends Controller with JsonUtils {

  def getProjects = Action.async { implicit request =>
    Project.findAll() map { projects =>
      val parsedProjects = projects map { p =>
        addSelfLink(Json.toJson(ProjectData.fromProject(p)), routes.Projects.getProject(p.id.toInt))
      }

      Ok(Json.obj("projects" -> parsedProjects))
    }
  }

  def getProject(id: Int) = Action.async { implicit request =>
    Project.find(id) map { projectOpt =>
      projectOpt map { p =>
        val projectJson = addSelfLink(Json.toJson(ProjectData.fromProject(p)), routes.Projects.getProject(id)) ++ Json.obj(
          "links" -> Seq(createLink("time-entries", routes.Projects.getTimeEntriesForProject(id)))
        )
        Ok(projectJson)
      } getOrElse NotFound(errorJson("The requested project could not be found"))
    }
  }

  def createProject = Action.async(parse.json) { implicit request =>
    request.body.validate[ProjectData].fold(
      errors => Future.successful(BadRequest(errorJson(errors))),
      projectData => {
        Project.create(projectData.projectName, projectData.projectDescription) map { p =>
          Created.withHeaders(LOCATION -> routes.Projects.getProject(p.id.toInt).absoluteURL())
        }
      }
    )
  }

  def editProject(id: Int) = Action.async(parse.json) { implicit request =>
    request.body.validate[ProjectData].fold(
      errors => Future.successful(BadRequest(errorJson(errors))),
      projectData => {
        Project.find(id) map { _.map { project =>
            Project.save(project)
            NoContent
          } getOrElse NotFound(errorJson("The requested project could not be found"))
        }
      }
    )
  }

  def deleteProject(id: Int) = Action.async(parse.empty) { implicit request =>
    Project.find(id) map { _.map { project =>
        // Delete all linked TimeEntries (DO THIS NORMALLY WITH A QUERY!)
        TimeEntry.findAllBy(sqls.eq(TimeEntry.te.projectId, id)).map(_.foreach(TimeEntry.destroy))

        // Destroy the project
        Project.destroy(project)
        NoContent
      } getOrElse NotFound(errorJson("The requested project could not be found"))
    }
  }

  def searchProjects(projectName: String) = Action.async { implicit request =>
    Project.findAllBy(sqls.like(Project.p.projectName, s"%$projectName%")) map { projects =>
      val parsedProjects = projects.map { project =>
        addSelfLink(Json.toJson(ProjectData.fromProject(project)), routes.Projects.getProject(project.id.toInt))
      }

      Ok(Json.obj("projects" -> parsedProjects))
    }
  }

  def getTimeEntriesForProject(id: Int) = Action.async { implicit request =>
    Project.find(id) flatMap  { projectOpt =>
      projectOpt map { project =>

        TimeEntry.findAllBy(sqls.eq(TimeEntry.te.projectId, id)) map { timeEntries =>
          val parsedTimeEntries = timeEntries.map { te =>
            addSelfLink(Json.toJson(TimeEntryData.fromTimeEntry(te)), routes.TimeEntries.getTimeEntry(te.id))
          }

          Ok(Json.obj("time-entries" -> parsedTimeEntries))
        }

      } getOrElse Future.successful(NotFound(errorJson("The requested project could not be found")))
    }
  }
}
