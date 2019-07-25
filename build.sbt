name := "HowardBot"

version := "0.1"

scalaVersion := "2.12.4"
scalacOptions += "-Ypartial-unification"

// Cats for functional stuff
libraryDependencies += "org.typelevel" %% "cats-core" % "2.0.0-M4"

// Telegram bot api
libraryDependencies ++= Seq(
  "com.bot4s" % "telegram-core_2.12" % "4.3.0-RC1",
  "com.bot4s" % "telegram-akka_2.12" % "4.3.0-RC1",
)

// Web server for homepage
libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-http"   % "10.1.8",
  "com.typesafe.akka" %% "akka-stream" % "2.5.19"
)

// Logging 
libraryDependencies += "biz.enef" %% "slogging" % "0.6.1"

// HTML generation
libraryDependencies += "com.lihaoyi" %% "scalatags" % "0.7.0"
  
// HTTP library
libraryDependencies += "com.softwaremill.sttp" %% "core" % "1.6.3"

// Json serialization
val circeVersion = "0.11.1"
libraryDependencies ++= Seq(
  "io.circe" %% "circe-core",
  "io.circe" %% "circe-generic",
  "io.circe" %% "circe-parser"
).map(_ % circeVersion) 

// Fake data generation
libraryDependencies += "it.bitbl" %% "scala-faker" % "0.4"

// Database management
lazy val doobieVersion = "0.7.0"
libraryDependencies ++= Seq(
  "org.tpolecat" %% "doobie-core"     % doobieVersion,
  "org.tpolecat" %% "doobie-postgres" % doobieVersion,
  "org.tpolecat" %% "doobie-specs2"   % doobieVersion
)

