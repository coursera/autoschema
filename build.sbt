name := "autoschema"

organization := "org.coursera"

scalaVersion := "2.11.7"

scalacOptions ++= Seq("-feature", "-unchecked", "-deprecation", "-encoding", "utf8")

testOptions += Tests.Argument(TestFrameworks.JUnit, "-q", "-v")

libraryDependencies ++= Seq(
  "com.typesafe.play" %% "play-json" % "2.3.10",
  "org.scalatest" %% "scalatest" % "2.1.7" % "test",
  "com.novocode" % "junit-interface" % "0.11" % "test")