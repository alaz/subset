import sbt._
import Keys._

object SubsetBuild extends Build {
  lazy val root =
    Project("subset", file("."))
      .configs(IntegrationTest)
      .settings(Defaults.itSettings :_*)
}
