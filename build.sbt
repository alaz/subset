// Unit tests (invoke with command `test`) do not depend on MongoDB running.
//
// Integration tests (invoke with command `it:test`) depend on
// MongoDB running. By default, they connect to `localhost`, database `test`
// and use collection `test` (the tests will drop it!).
//
// You may change the defaults in src/it/scala/ExampleFixture.scala

import com.jsuereth.sbtsite.SiteKeys
import ls.Plugin._

organization := "com.osinka.subset"

name := "subset"

homepage := Some(url("https://github.com/osinka/subset"))

startYear := Some(2011)

scalaVersion := "2.9.2"

licenses += "Apache License, Version 2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")

crossScalaVersions := Seq("2.8.1", "2.8.2", "2.9.1", "2.9.2")

organizationName := "Osinka"

description := """Subset: typed MongoDB fields and query/update builders"""

scalacOptions += "-unchecked"

parallelExecution in IntegrationTest := false

libraryDependencies ++= Seq(
  "org.mongodb" % "mongo-java-driver" % "2.9.3",
  "joda-time" % "joda-time" % "1.6.2" % "optional",
  "org.scalatest" %% "scalatest" % "1.8" % "it,test",
  "junit" % "junit" % "4.11" % "it,test"
)

seq(site.settings:_*)

site.addMappingsToSiteDir(mappings in packageDoc in Compile, "api")

SiteKeys.siteMappings <<=
  (SiteKeys.siteMappings, PamfletKeys.write, PamfletKeys.output) map { (mappings, _, dir) =>
    mappings ++ (dir ** "*.*" x relativeTo(dir))
  }

seq(ghpages.settings:_*)

git.remoteRepo := "git@github.com:osinka/subset.git"

credentials += Credentials(Path.userHome / ".ivy2" / "credentials_sonatype")

pomIncludeRepository := { x => false }

publishTo <<= (version) { version: String =>
  if (version.trim endsWith "SNAPSHOT")
    Some(Resolver.file("file", file(Path.userHome.absolutePath+"/.m2/repository")))
  else
    Some("Sonatype OSS Staging" at "https://oss.sonatype.org/service/local/staging/deploy/maven2/")
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
    <scm>
      <connection>scm:git:git://github.com/osinka/subset.git</connection>
      <developerConnection>scm:git:git@github.com:osinka/subset.git</developerConnection>
      <url>http://github.com/osinka/subset</url>
    </scm>
    <issueManagement>
      <system>github</system>
      <url>http://github.com/osinka/subset/issues</url>
    </issueManagement>
  </xml:group>
