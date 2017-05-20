name := "pppl-video"

organization := "edu.colorado.cs"

version := "3.4.3"

scalaVersion := "2.11.8"

scalacOptions ++= Seq("-unchecked", "-deprecation")

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4",
  "org.scalacheck" %% "scalacheck" % "1.13.4" % "test",
  "org.scalactic" %% "scalactic" % "3.0.0",
  "org.scalatest" %% "scalatest" % "3.0.0" % "test"
)

// set logging to show only errors during runs
logLevel in run := Level.Error

logLevel in runMain := Level.Error

// set scalatest options: -o standard output, D durations
testOptions in Test += Tests.Argument("-oD")

parallelExecution in Test := false