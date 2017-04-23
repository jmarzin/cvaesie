name := "cvaesie"

version := "1.0"

scalaVersion := "2.11.7"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.1" % "test"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"
libraryDependencies += "org.scala-lang.modules" %% "scala-swing" % "1.0.2"
unmanagedBase <<= baseDirectory { base => base / "libs" }