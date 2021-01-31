scalaVersion := "2.13.4"

scalacOptions ++= Seq(
  "-deprecation",
  "-feature",
  "-Ymacro-annotations",
)

name := "bootcamp_homeworks"
version := "1.0"

libraryDependencies ++= Seq(
  "org.scalactic" %% "scalactic" % "3.2.2",
  "org.scalatest" %% "scalatest" % "3.2.2" % "test"
)
