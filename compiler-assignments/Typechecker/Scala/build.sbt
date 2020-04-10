name := "CPPParser"
version := "0.1"
scalaVersion := "2.12.8"

val projectMainClass = "CPPScala.Test"
mainClass in (Compile, run) := Some(projectMainClass)

scalacOptions += "-Ypartial-unification"
libraryDependencies += "org.typelevel" %% "cats-core" % "1.6.0"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"
