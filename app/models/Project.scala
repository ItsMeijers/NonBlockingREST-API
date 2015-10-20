package models

import play.api.libs.json._
import scalikejdbc._, async._, FutureImplicits._
import scalikejdbc.interpolation.SQLSyntax._
import scala.concurrent._

case class ProjectData(projectName: String, projectDescription: String)

object ProjectData{
  implicit val projectDataFormats = Json.format[ProjectData]

  def fromProject(p: Project): ProjectData = ProjectData(p.projectName, p.projectDescription)
}

case class Project(
  id: Int,
  projectName: String,
  projectDescription: String)

object Project extends SQLSyntaxSupport[Project] with ShortenedNames {

  override val tableName = "project"
  override val columns   = Seq("id", "project_name", "project_description")

  def apply(p: SyntaxProvider[Project])(rs: WrappedResultSet): Project = apply(p.resultName)(rs)
  def apply(p: ResultName[Project])(rs: WrappedResultSet): Project = new Project(
    id                 = rs.get(p.id),
    projectName        = rs.get(p.projectName),
    projectDescription = rs.get(p.projectDescription)
  )

  val p  = Project.syntax("p")
  val te = TimeEntry.syntax

  def find(id: Int)(implicit session: AsyncDBSession = AsyncDB.sharedSession, cxt: EC = ECGlobal): Future[Option[Project]] =
    withSQL {
      select.from(Project as p).where.eq(p.id, id)
    } map Project(p)

  def findAll()(implicit session: AsyncDBSession = AsyncDB.sharedSession, cxt: EC = ECGlobal): Future[List[Project]] =
    withSQL(select.from(Project as p)).map(Project(p))

  def findByEmployee(employeeId: Int)(implicit session: AsyncDBSession = AsyncDB.sharedSession, cxt: EC = ECGlobal): Future[List[Project]] =
    withSQL{
      select(distinct(p.resultAll)).from(Project as p)
        .innerJoin(TimeEntry as te)
        .on(p.id, te.projectId)
        .where.eq(te.employeeId, employeeId)
    } map Project(p)

  def countAll()(implicit session: AsyncDBSession = AsyncDB.sharedSession, cxt: EC = ECGlobal): Future[Long] =
    withSQL(select(sqls"count(1)").from(Project as p)).map(_.long(1)).single.future.map(_.get)

  def findBy(where: SQLSyntax)(implicit session: AsyncDBSession = AsyncDB.sharedSession, cxt: EC = ECGlobal): Future[Option[Project]] =
    withSQL {
      select.from(Project as p).where.append(sqls"${where}")
    } map Project(p)

  def findAllBy(where: SQLSyntax)(implicit session: AsyncDBSession = AsyncDB.sharedSession, cxt: EC = ECGlobal): Future[List[Project]] =
    withSQL {
      select.from(Project as p).where.append(sqls"${where}")
    } map Project(p)

  def countBy(where: SQLSyntax)(implicit session: AsyncDBSession = AsyncDB.sharedSession, cxt: EC = ECGlobal): Future[Long] =
    withSQL {
      select(sqls"count(1)").from(Project as p).where.append(sqls"${where}")
    }.map(_.long(1)).single.future.map(_.get)

  def create(data: ProjectData): Future[Project] = create(data.projectName, data.projectDescription)

  def create(
    projectName: String,
    projectDescription: String)(implicit session: AsyncDBSession = AsyncDB.sharedSession, cxt: EC = ECGlobal): Future[Project] =
      for {
        id <- withSQL {
          insert.into(Project).namedValues(
            column.projectName        -> projectName,
            column.projectDescription -> projectDescription
          ).returningId
        }.updateAndReturnGeneratedKey.future
      } yield Project(id = id.toInt, projectName = projectName, projectDescription = projectDescription)

  def save(entity: Project)(implicit session: AsyncDBSession = AsyncDB.sharedSession, cxt: EC = ECGlobal): Future[Project] =
    withSQL {
      update(Project).set(
        column.id                 -> entity.id,
        column.projectName        -> entity.projectName,
        column.projectDescription -> entity.projectDescription
      ).where.eq(column.id, entity.id)
    }.update.future.map(_ => entity)

  def destroy(entity: Project)(implicit session: AsyncDBSession = AsyncDB.sharedSession, cxt: EC = ECGlobal): Unit =
    withSQL { delete.from(Project).where.eq(column.id, entity.id) }.update()

}

