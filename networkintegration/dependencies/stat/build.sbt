scalaVersion := "2.13.12"

name := "stat"



scalacOptions ++= Seq("-language:postfixOps")

evictionErrorLevel := Level.Warn
libraryDependencies ++= Seq(
  "io.github.pityka" %% "saddle-core" % "4.0.0-M11",
  "io.github.pityka" %% "saddle-binary" % "4.0.0-M11",
  "io.github.pityka" %% "saddle-linalg" % "4.0.0-M11",
  "io.github.pityka" %% "saddle-ops-inlined" % "4.0.0-M11",
  "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.1",
  "biz.enef" %% "slogging" % "0.6.2",
    "org.scalatest" %% "scalatest" % "3.2.5" % "test",
)

fork := true

parallelExecution in Test := false

lazy val root = project in file(".")

useSuperShell in ThisBuild := false
