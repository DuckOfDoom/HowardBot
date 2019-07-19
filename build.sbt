name := "HowardBot"

version := "0.1"

scalaVersion := "2.12.4"
scalacOptions += "-Ypartial-unification"

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % "2.0.0-M4",
  
  "com.bot4s" % "telegram-core_2.12" % "4.3.0-RC1",
  "com.bot4s" % "telegram-akka_2.12" % "4.3.0-RC1",

  "biz.enef" %% "slogging" % "0.6.1",
  
  "com.lihaoyi" %% "scalatags" % "0.7.0",
  
  "com.softwaremill.sttp" %% "core" % "1.6.3",
)

val circeVersion = "0.11.1"
libraryDependencies ++= Seq(
  "io.circe" %% "circe-core",
  "io.circe" %% "circe-generic",
  "io.circe" %% "circe-parser"
).map(_ % circeVersion)

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-http"   % "10.1.8",
  "com.typesafe.akka" %% "akka-stream" % "2.5.19"
)
