scalaVersion := "2.13.10"

lazy val commonSettings = Seq(
  scalaVersion := "2.13.10",
  scalacOptions ++= Seq("-language:postfixOps"),
  javacOptions ++= Seq("-Xdoclint:none"),
  licenses += ("MIT", url("http://opensource.org/licenses/MIT"))
)

lazy val core = project
  .in(file("core"))
  .settings(commonSettings)
  .settings(
    name := "nspl-core"
  )
  .enablePlugins(spray.boilerplate.BoilerplatePlugin)

lazy val coreJS = project
  .in(file("core"))
  .settings(commonSettings)
  .settings(
    name := "nspl-core-js",
    target := file("core/targetJS"),
    sourceManaged in Compile := (sourceManaged in Compile).value.getAbsoluteFile
  )
  .enablePlugins(ScalaJSPlugin)
  .enablePlugins(spray.boilerplate.BoilerplatePlugin)

lazy val sharedJs = project
  .in(file("shared-js"))
  .settings(commonSettings)
  .settings(
    name := "nspl-shared-js",
    libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "2.1.0"
  )
  .enablePlugins(ScalaJSPlugin)
  .dependsOn(coreJS)

lazy val canvas = project
  .in(file("canvas"))
  .settings(commonSettings)
  .enablePlugins(ScalaJSPlugin)
  .dependsOn(coreJS, sharedJs)

lazy val sharedJvm = project
  .in(file("shared-jvm"))
  .settings(commonSettings)
  .settings(
    name := "nspl-shared-jvm"
  )
  .dependsOn(core)

lazy val awt = project
  .in(file("awt"))
  .settings(commonSettings)
  .settings(
    name := "nspl-awt",
    libraryDependencies ++= Seq(
      "de.erichseifert.vectorgraphics2d" % "VectorGraphics2D" % "0.13",
      "org.scalatest" %% "scalatest" % "3.2.5" % "test"
    )
  )
  .dependsOn(core, sharedJvm)



lazy val saddle = (project in file("saddle"))
  .settings(commonSettings)
  .settings(
    name := "nspl-saddle",
    fork := true,
    libraryDependencies ++= Seq(
      "io.github.pityka" %% "saddle-core" % "4.0.0-M11",
      "org.scalatest" %% "scalatest" % "3.2.5" % "test"
    )
  )
  .dependsOn(core, awt)

lazy val saddleJS = (project in file("saddle"))
  .settings(commonSettings)
  .settings(
    name := "nspl-saddle-js",
    target := file("saddle/targetJS"),
    name := "nspl-saddle-js",
    libraryDependencies ++= Seq(
      "io.github.pityka" %%% "saddle-core" % "4.0.0-M11"
    )
  )
  .dependsOn(coreJS)
  .enablePlugins(ScalaJSPlugin)
  .enablePlugins(spray.boilerplate.BoilerplatePlugin)

publishArtifact := false

lazy val root = (project in file("."))
  .settings(commonSettings: _*)
  .settings(publish / skip := true)
  .aggregate(
    saddle,
    saddleJS,
    awt,
    canvas,
    sharedJs,
    sharedJvm,
    core,
    coreJS,
  )



