name := "HowardBot"

version := "0.1"

scalaVersion := "2.12.4"
scalacOptions += "-Ypartial-unification"

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % "2.0.0-M4",
  
  "com.bot4s" % "telegram-core_2.12" % "4.3.0-RC1",
  "com.bot4s" % "telegram-akka_2.12" % "4.3.0-RC1",
)

val circeVersion = "0.11.1"
libraryDependencies ++= Seq(
  "io.circe" %% "circe-core",
  "io.circe" %% "circe-generic",
  "io.circe" %% "circe-parser"
).map(_ % circeVersion)

val finchVersion = "0.29.0"
libraryDependencies ++= Seq(
  "com.github.finagle" %% "finchx-core",
  "com.github.finagle" %% "finchx-circe",
).map(_ % finchVersion)


