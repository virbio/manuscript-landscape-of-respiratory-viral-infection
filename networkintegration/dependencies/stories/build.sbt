scalaVersion := "2.13.10"

lazy val commonSettings = Seq(
  scalaVersion := "2.13.10",
  javacOptions ++= Seq("-Xdoclint:none"),
  scalacOptions ++= Seq("-language:postfixOps")
)

val nspl = RootProject(file("../nspl"))
val nsplAwt = ProjectRef(file("../nspl"), "awt")
val nsplSaddle = ProjectRef(file("../nspl"), "saddle")
val nsplSaddleJS = ProjectRef(file("../nspl"), "saddleJS")
val nsplCanvasJs = ProjectRef(file("../nspl"), "canvas")

val shared =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Pure)
    .settings(
      name := "stories-shared",
      libraryDependencies ++= Seq(
        "io.circe" %%% "circe-generic" % "0.14.6",
        "io.github.pityka" %%% "saddle-circe" % "4.0.0-M11"
      )
    )
    .settings(commonSettings)

val sharedJS = shared.js
val sharedJVM = shared.jvm

lazy val core = project
  .in(file("core"))
  .settings(commonSettings)
  .settings(
    name := "stories-core"
  )
  .settings(
    libraryDependencies ++= Seq(
      "io.github.pityka" %% "fileutils" % "1.2.3",
      "com.vladsch.flexmark" % "flexmark" % "0.62.2",
      "io.circe" %%% "circe-generic" % "0.14.6",
      "org.scalatest" %% "scalatest" % "3.2.5" % "test"
    )
  )
  .settings(
    artifactName in packageSrc in Compile := { (sv: ScalaVersion, module: ModuleID, artifact: Artifact) =>
      "sources.jar"
    },
    resources in Test += (packageSrc in Compile).value
  )
  .settings(
    resources in Compile += (fullOptJS in Compile in js).value.data
  )
  .dependsOn(sharedJVM, nsplAwt, nsplSaddle)

lazy val js = project
  .in(file("js"))
  .settings(commonSettings)
  .settings(
    name := "stories-js"
  )
  .settings(
    scalaJSUseMainModuleInitializer := false,
    libraryDependencies ++= Seq(
      "org.portable-scala" %%% "portable-scala-reflect" % "1.1.1",
      "io.circe" %%% "circe-parser" % "0.14.6",
      "org.scala-js" %%% "scalajs-dom" % "2.1.0",
      "com.raquo" %%% "laminar" % "16.0.0"
    )
  )
  .dependsOn(sharedJS)
  .dependsOn(nsplSaddleJS, nsplCanvasJs)
  .enablePlugins(ScalaJSPlugin)
  
publishArtifact := false

fork := true

lazy val root = (project in file("."))
  .settings(commonSettings: _*)
  .settings(publish / skip := true)
  .aggregate(core, js, sharedJS, sharedJVM)
