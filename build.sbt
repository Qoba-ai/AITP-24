Compile / run / fork := false


lazy val root = project
  .in(file("."))
  .settings(
    name := "AITP24",
    version := "0.1.1",
    scalaVersion := "3.5.0",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.12" % "test",
    libraryDependencies += "be.adamv" %% "llgraph" % "0.3.5",
  )

publishTo := Some(Resolver.file("local-ivy", file("~")))
