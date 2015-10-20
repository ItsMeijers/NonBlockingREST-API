package models

import org.joda.time.DateTime
import org.joda.time.format.ISODateTimeFormat
import play.api.libs.json._
import scalikejdbc._, async._, FutureImplicits._
import scalikejdbc.interpolation.SQLSyntax._
import scala.concurrent._

case class TimeEntryData(
  startTime  : DateTime,
  endTime    : DateTime,
  employeeId : Int,
  projectId  : Int,
  comment    : Option[String])

object TimeEntryData {

  private lazy val ISODateTimeFormatter = ISODateTimeFormat.dateTimeNoMillis
  private lazy val ISODateTimeParser    = ISODateTimeFormat.dateTimeParser

  implicit val DateTimeFormatter = new Format[DateTime] {
    def reads(j: JsValue)   = JsSuccess(ISODateTimeParser.parseDateTime(j.as[String]))
    def writes(o: DateTime) = JsString(ISODateTimeFormatter.print(o))
  }

  implicit val timEntryFormats = Json.format[TimeEntryData]

  def fromTimeEntry(te: TimeEntry): TimeEntryData =
    TimeEntryData(te.startTime, te.endTime, te.employeeId, te.projectId, te.comment)

}

case class TimeEntry(
  id         : Int,
  startTime  : DateTime,
  endTime    : DateTime,
  employeeId : Int,
  projectId  : Int,
  comment    : Option[String] = None)

object TimeEntry extends SQLSyntaxSupport[TimeEntry] with ShortenedNames {

  override val tableName = "timeentry"
  override val columns   = Seq("id", "start_time", "end_time", "employee_id", "project_id", "comment")

  def apply(te: SyntaxProvider[TimeEntry])(rs: WrappedResultSet): TimeEntry = apply(te.resultName)(rs)
  def apply(te: ResultName[TimeEntry])(rs: WrappedResultSet): TimeEntry = new TimeEntry(
    id         = rs.get(te.id),
    startTime  = rs.get(te.startTime),
    endTime    = rs.get(te.endTime),
    employeeId = rs.get(te.employeeId),
    projectId  = rs.get(te.projectId),
    comment    = rs.get(te.comment)
  )

  lazy val te = TimeEntry.syntax("te")
  lazy val e  = Employee.syntax

  def find(id: Int)(implicit session: AsyncDBSession = AsyncDB.sharedSession, cxt: EC = ECGlobal): Future[Option[TimeEntry]] =
    withSQL {
      select.from(TimeEntry as te).where.eq(te.id, id)
    } map TimeEntry(te)


  def findAll()(implicit session: AsyncDBSession = AsyncDB.sharedSession, cxt: EC = ECGlobal): Future[List[TimeEntry]] =
    withSQL(select.from(TimeEntry as te)).map(TimeEntry(te))


  def findFromAndTo(fromOpt: Option[DateTime], toOpt: Option[DateTime])
                   (implicit session: AsyncDBSession = AsyncDB.sharedSession, cxt: EC = ECGlobal): Either[String, Future[List[TimeEntry]]] =
    if(fromOpt.isEmpty && toOpt.isEmpty)
      Left("No query parameters supplied")
    else Right(
      withSQL {
        select.from(TimeEntry as te)
          .where(sqls.toAndConditionOpt(
          fromOpt.map(from => sqls.gt(te.startTime, from)),
          toOpt.map(to => sqls.lt(te.endTime, to))
        )).orderBy(te.startTime desc)
      } map TimeEntry(te))


  def findByEmployee(id: Int)(implicit session: AsyncDBSession = AsyncDB.sharedSession, cxt: EC = ECGlobal): Future[List[TimeEntry]] =
    withSQL{
      select(distinct(te.resultAll)).from(TimeEntry as te)
        .innerJoin(Employee as e)
        .on(e.id, te.employeeId)
        .where.eq(e.id, id)
        .orderBy(te.startTime desc)
    }.map(TimeEntry(te))


  def countAll()(implicit session: AsyncDBSession = AsyncDB.sharedSession, cxt: EC = ECGlobal): Future[Long] =
    withSQL {
     select(sqls.count).from(TimeEntry as te)
    }.map(_.long(1)).single.future.map(_.get)


  def findBy(where: SQLSyntax)(implicit session: AsyncDBSession = AsyncDB.sharedSession, cxt: EC = ECGlobal): Future[Option[TimeEntry]] =
    withSQL {
      select.from(TimeEntry as te).where.append(sqls"$where")
    } map TimeEntry(te)


  def findAllBy(where: SQLSyntax)(implicit session: AsyncDBSession = AsyncDB.sharedSession, cxt: EC = ECGlobal): Future[List[TimeEntry]] =
    withSQL {
      select.from(TimeEntry as te).where.append(sqls"$where")
    } map TimeEntry(te)


  def countBy(where: SQLSyntax)(implicit session: AsyncDBSession = AsyncDB.sharedSession, cxt: EC = ECGlobal): Future[Long] =
    withSQL {
      select(sqls"count(1)").from(TimeEntry as te).where.append(sqls"${where}")
    }.map(rs => rs.long(1)).single.future.map(_.get)


  def create(ted: TimeEntryData): Future[TimeEntry] = create(ted.startTime, ted.endTime, ted.employeeId, ted.projectId, ted.comment)

  def create(
    startTime  : DateTime,
    endTime    : DateTime,
    employeeId : Int,
    projectId  : Int,
    comment    : Option[String] = None)(implicit session: AsyncDBSession = AsyncDB.sharedSession, cxt: EC = ECGlobal): Future[TimeEntry] =
      for {
        id <- withSQL {
          insert.into(TimeEntry).namedValues(
            column.startTime  -> startTime,
            column.endTime    -> endTime,
            column.employeeId -> employeeId,
            column.projectId  -> projectId,
            column.comment    -> comment
          ).returningId
        }.updateAndReturnGeneratedKey.future
      } yield TimeEntry(
        id         = id.toInt,
        startTime  = startTime,
        endTime    = endTime,
        employeeId = employeeId,
        projectId  = projectId,
        comment    = comment)

  def save(entity: TimeEntry)(implicit session: AsyncDBSession = AsyncDB.sharedSession, cxt: EC = ECGlobal): Future[TimeEntry] =
    withSQL {
      update(TimeEntry).set(
        column.id         -> entity.id,
        column.startTime  -> entity.startTime,
        column.endTime    -> entity.endTime,
        column.employeeId -> entity.employeeId,
        column.projectId  -> entity.projectId,
        column.comment    -> entity.comment
      ).where.eq(column.id, entity.id)
    }.update.future.map(_ => entity)


  def destroy(entity: TimeEntry)(implicit session: AsyncDBSession = AsyncDB.sharedSession, cxt: EC = ECGlobal): Unit =
    withSQL { delete.from(TimeEntry).where.eq(column.id, entity.id) }.update()


}


