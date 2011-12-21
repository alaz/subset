import sbtrelease.Release._

organization := "com.osinka.subset"

name := "subset"

version := "0.3.0-SNAPSHOT"

scalaVersion := "2.8.2"

libraryDependencies ++= Seq(
  "org.mongodb" % "mongo-java-driver" % "2.7.2",
  "joda-time" % "joda-time" % "1.6.2" % "optional",
  "org.scalatest" %% "scalatest" % "1.5.1" % "test",
  "junit" % "junit" % "4.10" % "test"
)

seq(releaseSettings: _*)

seq(site.settings:_*)

seq(ghpages.settings:_*)

git.remoteRepo := "git@github.com:osinka/subset.git"

// https://github.com/OlegIlyenko/scaldi/blob/master/build.sbt

pomExtra := <xml:group>
    <name>Subset - MongoDB Document data serialization and query builder library</name>
    <inceptionYear>2011</inceptionYear>
    <description>
      Minimalistic library for
      - building typed field serializers/deserializers to/from DBObject
      - typed query builders
    </description>
    <organization>
      <name>Osinka.com</name>
      <url>http://www.osinka.com</url>
    </organization>
    <licenses>
      <license>
        <name>Apache License, Version 2.0</name>
        <url>http://www.apache.org/licenses/LICENSE-2.0</url>
      </license>
    </licenses>
    <developers>
      <developer>
        <id>alaz</id>
        <email>azarov@osinka.com</email>
        <name>Alexander Azarov</name>
        <timezone>+4</timezone>
      </developer>
    </developers>
    <issueManagement>
      <system>github</system>
      <url>http://github.com/osinka/subset/issues</url>
    </issueManagement>
  </xml:group>