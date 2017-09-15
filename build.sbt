name := "autoschema"

organization := "org.coursera"

scalaVersion := "2.12.2"

scalacOptions ++= Seq("-feature", "-unchecked", "-deprecation", "-encoding", "utf8")

testOptions += Tests.Argument(TestFrameworks.JUnit, "-q", "-v")

libraryDependencies ++= Seq(
  "com.typesafe.play" %% "play-json" % "2.6.3",
  "org.scalatest" %% "scalatest" % "3.0.4" % "test",
  "com.novocode" % "junit-interface" % "0.11" % "test")