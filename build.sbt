import play.PlayImport.PlayKeys._

name := """NonBlockingRESTAPI"""

version := "1.0-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayScala)

scalaVersion := "2.11.6"

libraryDependencies ++= Seq(
  jdbc,
  anorm,
  cache,
  ws,
  "org.scalikejdbc"     %% "scalikejdbc-async"             % "0.5.5",
  "org.scalikejdbc"     %% "scalikejdbc-async-play-plugin" % "0.5.5",
  "com.github.mauricio" %% "postgresql-async"              % "0.2.16",
  "org.postgresql"       %  "postgresql"                   % "9.3-1102-jdbc41",
  "com.github.tototoshi" %% "play-flyway"                  % "1.2.0"
)

routesImport += "utils.QueryBinders._"