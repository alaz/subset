import com.jsuereth.sbtsite.SiteKeys
import sbtrelease.Release._

organization := "com.osinka.subset"

name := "subset"

startYear := Some(2011)

version := "0.3.1-SNAPSHOT"

scalaVersion := "2.9.1"

crossScalaVersions := Seq("2.8.2", "2.9.1")

organizationName := "Osinka"

description := """Subset is a tiny and simple library for
  - typed field serializers/deserializers to/from DBObject
  - typed query/update builders"""

libraryDependencies ++= Seq(
  "org.mongodb" % "mongo-java-driver" % "2.7.2",
  "joda-time" % "joda-time" % "1.6.2" % "optional",
  "junit" % "junit" % "4.10" % "test"
)

libraryDependencies <+= scalaVersion({
  case v: String if v.startsWith("2.8") =>
    "org.scalatest" %% "scalatest" % "1.5.1" % "test"
  case v: String if v.startsWith("2.9") =>
    "org.scalatest" %% "scalatest" % "1.6.1" % "test"
  case v =>
    error("Unsupported Scala version "+v)
})

publishTo <<= (version) { version: String =>
  if (version.trim endsWith "SNAPSHOT")
    Some(Resolver.file("file", file(Path.userHome.absolutePath+"/.m2/repository")))
  else
    Some("Scala Tools Nexus" at "http://nexus.scala-tools.org/content/repositories/releases/")
}

seq(releaseSettings: _*)

seq(site.settings:_*)

seq(ghpages.settings:_*)

git.remoteRepo := "git@github.com:osinka/subset.git"

site.addMappingsToSiteDir(mappings in packageDoc in Compile, "api")

SiteKeys.siteMappings <<=
  (SiteKeys.siteMappings, PamfletKeys.write, PamfletKeys.output) map { (mappings, _, dir) =>
    mappings ++ (dir ** "*.*" x relativeTo(dir))
  }

pomExtra := <xml:group>
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
