organization := "com.osinka.subset"

name := "subset"

version := "0.2.0-SNAPSHOT"

scalaVersion := "2.8.2"

libraryDependencies ++= Seq(
  "org.mongodb" % "mongo-java-driver" % "2.7.2",
  "net.liftweb" % "lift-util_2.8.1" % "2.3",
  "joda-time" % "joda-time" % "1.6.2",
  "org.scalatest" %% "scalatest" % "1.5.1" % "test",
  "junit" % "junit" % "4.10" % "test"
)
