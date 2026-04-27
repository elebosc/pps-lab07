ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := "3.3.7"
ThisBuild / scalafixDependencies += "com.github.liancheng" %% "organize-imports" % "0.6.0"

lazy val root = (project in file("."))
  .settings(
    name := "pps-lab7",
    libraryDependencies += "com.github.sbt" % "junit-interface" % "0.13.3" % Test,
    // add scala test
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.18" % Test,
    semanticdbEnabled := true,
    semanticdbVersion := scalafixSemanticdb.revision,
    scalacOptions += "-Wunused:all"
  )
