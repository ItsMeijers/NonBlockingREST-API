package models

import org.joda.time.LocalDate
import play.api.libs.json._
import scalikejdbc._, async._, FutureImplicits._
import scala.concurrent._

case class EmployeeData(
  firstName : String,
  prefix    : Option[String] = None,
  lastName  : String,
  jobTitle  : String,
  birthDate : LocalDate)

object EmployeeData {
  implicit val employeeDataFormats = Json.format[EmployeeData]

  def fromEmployee(e: Employee): EmployeeData = EmployeeData(e.firstName, e.prefix, e.lastName, e.jobTitle, e.birthDate)
}

case class Employee(
  id        : Int,
  firstName : String,
  prefix    : Option[String] = None,
  lastName  : String,
  jobTitle  : String,
  birthDate : LocalDate)

object Employee extends SQLSyntaxSupport[Employee] with ShortenedNames {

  override val tableName = "employee"
  override val columns   = Seq("id", "first_name", "prefix", "last_name", "job_title", "birth_date")

  def apply(e: SyntaxProvider[Employee])(rs: WrappedResultSet): Employee = apply(e.resultName)(rs)
  def apply(e: ResultName[Employee])(rs: WrappedResultSet): Employee = new Employee(
    id        = rs.get(e.id),
    firstName = rs.get(e.firstName),
    prefix    = rs.get(e.prefix),
    lastName  = rs.get(e.lastName),
    jobTitle  = rs.get(e.jobTitle),
    birthDate = rs.get(e.birthDate)
  )

  val e = Employee.syntax("e")

  def find(id: Int)(implicit session: AsyncDBSession = AsyncDB.sharedSession, cxt: EC = ECGlobal): Future[Option[Employee]] =
    withSQL {
      select.from(Employee as e).where.eq(e.id, id)
    } map Employee(e)

  def findAll()(implicit session: AsyncDBSession = AsyncDB.sharedSession, cxt: EC = ECGlobal): Future[List[Employee]] =
    withSQL(select.from(Employee as e)) map Employee(e)

  def findByNameAndJobTitle(nameOpt: Option[String], jobTitleOpt: Option[String])
                           (implicit session: AsyncDBSession = AsyncDB.sharedSession, cxt: EC = ECGlobal): Either[String, Future[List[Employee]]] =
    if(nameOpt.isEmpty && jobTitleOpt.isEmpty) Left("No query parameters supplied")
    else Right(
      withSQL {
        select.from(Employee as e)
          .where(sqls.toAndConditionOpt(
          nameOpt.map(name => sqls.eq(e.firstName, name)),
          jobTitleOpt.map(jobTitle => sqls.eq(e.jobTitle, jobTitle))
        ))
      } map Employee(e)
    )

  def countAll()(implicit session: AsyncDBSession = AsyncDB.sharedSession, cxt: EC = ECGlobal): Future[Long] =
    withSQL(select(sqls"count(1)").from(Employee as e)).map(rs => rs.long(1)).map(_.long(1)).single.future.map(_.get)

  def findBy(where: SQLSyntax)(implicit session: AsyncDBSession = AsyncDB.sharedSession, cxt: EC = ECGlobal): Future[Option[Employee]] =
    withSQL {
      select.from(Employee as e).where.append(sqls"${where}")
    } map Employee(e)


  def findAllBy(where: SQLSyntax)(implicit session: AsyncDBSession = AsyncDB.sharedSession, cxt: EC = ECGlobal): Future[List[Employee]] =
    withSQL {
      select.from(Employee as e).where.append(sqls"${where}")
    }.map(Employee(e))


  def countBy(where: SQLSyntax)(implicit session: AsyncDBSession = AsyncDB.sharedSession, cxt: EC = ECGlobal): Future[Long] =
    withSQL {
      select(sqls"count(1)").from(Employee as e).where.append(sqls"${where}")
    }.map(_.long(1)).single.future.map(_.get)

  def create(ed: EmployeeData): Future[Employee] = create(ed.firstName, ed.prefix, ed.lastName, ed.jobTitle, ed.birthDate)

  def create(
    firstName: String,
    prefix: Option[String] = None,
    lastName: String,
    jobTitle: String,
    birthDate: LocalDate)(implicit session: AsyncDBSession = AsyncDB.sharedSession, cxt: EC = ECGlobal): Future[Employee] =
      for {
        id <- withSQL {
          insert.into(Employee).namedValues(
            column.firstName -> firstName,
            column.prefix    -> prefix,
            column.lastName  -> lastName,
            column.jobTitle  -> jobTitle,
            column.birthDate -> birthDate
          ).returningId
        }.updateAndReturnGeneratedKey.future
      } yield Employee(
        id        = id.toInt,
        firstName = firstName,
        prefix    = prefix,
        lastName  = lastName,
        jobTitle  = jobTitle,
        birthDate = birthDate)

  def save(entity: Employee)(implicit session: AsyncDBSession = AsyncDB.sharedSession, cxt: EC = ECGlobal): Future[Employee] =
    withSQL {
      update(Employee).set(
        column.id        -> entity.id,
        column.firstName -> entity.firstName,
        column.prefix    -> entity.prefix,
        column.lastName  -> entity.lastName,
        column.jobTitle  -> entity.jobTitle,
        column.birthDate -> entity.birthDate
      ).where.eq(column.id, entity.id)
    }.update.future.map(_ => entity)

  def destroy(entity: Employee)(implicit session: AsyncDBSession = AsyncDB.sharedSession, cxt: EC = ECGlobal): Unit =
    withSQL { delete.from(Employee).where.eq(column.id, entity.id) }.update()

}
