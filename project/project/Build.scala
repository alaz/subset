import sbt._

object PluginDef extends Build {
  override def projects = Seq(root)
  lazy val root = Project("plugins", file(".")) dependsOn (ghpages, pamflet, release)

  lazy val ghpages = uri("git://github.com/jsuereth/xsbt-ghpages-plugin.git")
  lazy val release = uri("git://github.com/gseitz/sbt-release#v0.4")
  lazy val pamflet = uri("git://github.com/n8han/pamflet-plugin#0.4.0")
}
