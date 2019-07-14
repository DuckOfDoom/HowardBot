name := "HowardBot"

version := "0.1"

scalaVersion := "2.12.4"
scalacOptions += "-Ypartial-unification"

libraryDependencies += "org.typelevel" %% "cats-core" % "2.0.0-M4"
libraryDependencies += "com.bot4s" % "telegram-core_2.12" % "4.3.0-RC1"
libraryDependencies += "com.bot4s" % "telegram-akka_2.12" % "4.3.0-RC1"