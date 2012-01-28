// Unit tests (invoke with command `test`) do not depend on MongoDB running.
//
// Integration tests (invoke with command `it:test`) depend on
// MongoDB running. By default, they connect to `localhost`, database `test`
// and use collection `test` (the tests will drop it!).
//
// You may change the defaults in src/it/scala/ExampleFixture.scala

import com.jsuereth.sbtsite.SiteKeys
import sbtrelease.Release._
import ls.Plugin._

organization := "com.osinka.subset"

name := "subset"

startYear := Some(2011)

scalaVersion := "2.9.1"

licenses += "Apache License, Version 2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")

crossScalaVersions := Seq("2.8.1", "2.8.2", "2.9.1")

organizationName := "Osinka"

description := """Subset: typed MongoDB fields and query/update builders"""

scalacOptions += "-unchecked"

libraryDependencies ++= Seq(
  "org.mongodb" % "mongo-java-driver" % "2.7.2",
  "joda-time" % "joda-time" % "1.6.2" % "optional",
  "junit" % "junit" % "4.10" % "it,test"
)

libraryDependencies <+= scalaVersion({
  case v: String if v.startsWith("2.8") =>
    "org.scalatest" %% "scalatest" % "1.5.1" % "it,test"
  case v: String if v.startsWith("2.9") =>
    "org.scalatest" %% "scalatest" % "1.6.1" % "it,test"
  case v =>
    error("Unsupported Scala version "+v)
})

seq(site.settings:_*)

site.addMappingsToSiteDir(mappings in packageDoc in Compile, "api")

SiteKeys.siteMappings <<=
  (SiteKeys.siteMappings, PamfletKeys.write, PamfletKeys.output) map { (mappings, _, dir) =>
    mappings ++ (dir ** "*.*" x relativeTo(dir))
  }

seq(ghpages.settings:_*)

git.remoteRepo := "git@github.com:osinka/subset.git"

seq(releaseSettings: _*)

credentials += Credentials(Path.userHome / ".ivy2" / "credentials")

parallelExecution in IntegrationTest := false

publishTo <<= (version) { version: String =>
  if (version.trim endsWith "SNAPSHOT")
    Some(Resolver.file("file", file(Path.userHome.absolutePath+"/.m2/repository")))
  else
    Some("Scala Tools Nexus" at "http://nexus.scala-tools.org/content/repositories/releases/")
}

seq(lsSettings: _*)

(LsKeys.tags in LsKeys.lsync) := Seq("mongo", "mongodb")

(LsKeys.docsUrl in LsKeys.lsync) := Some(url("http://osinka.github.com/subset/Subset.html"))

pomExtra := <xml:group>
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
