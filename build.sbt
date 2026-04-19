ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.8.3"

lazy val root = (project in file("."))
  .settings(
    name := "parsercombinator",
    libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "2.4.0"
  )