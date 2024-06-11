scalaVersion := "2.13.8"

ThisBuild/evictionErrorLevel := Level.Warn

lazy val commonSettings = Seq(
  organization := "vir",
  scalaVersion := "2.13.8",
  javacOptions ++= Seq("-Xdoclint:none"),
  scalacOptions ++= Seq("-language:postfixOps"),
  fork := true,
  git.useGitDescribe := true,
  run / javaOptions ++= Seq("-XX:+UseSerialGC", "-Xmx20G"),
  Compile / packageDoc / publishArtifact := false,
  Compile / packageSrc / publishArtifact := false
)

val stat = RootProject(file("dependencies/stat"))
val nspl = RootProject(file("dependencies/nspl"))
val nsplAwt = ProjectRef(file("dependencies/nspl"),"awt")
val nsplSaddle = ProjectRef(file("dependencies/nspl"),"saddle")
val storiesCore = ProjectRef(file("dependencies/stories"),"core")


lazy val core = project
  .in(file("core"))
  .settings(commonSettings)
  .settings(
    name := "hdt-core",
    libraryDependencies ++= Seq(
      "com.typesafe.akka" %% "akka-actor" % "2.6.19",
      "com.typesafe.akka" %% "akka-remote" % "2.6.19",
      "com.typesafe.akka" %% "akka-slf4j" % "2.6.19",
      "io.github.pityka" %% "fileutils" % "1.2.5",
      "io.github.pityka" %% "saddle-core" % "4.0.0-M11",
      "io.github.pityka" %% "saddle-binary" % "4.0.0-M11",
      "com.github.scopt" %% "scopt" % "4.0.0-RC2",
      "io.github.pityka" %% "tasks-core" % "3.0.0-M8",
      "io.github.pityka" %% "tasks-circe" % "3.0.0-M8",
      "com.typesafe" % "config" % "1.4.0",
      "org.slf4j" % "slf4j-nop" % "1.7.28",
      "com.github.haifengl" % "smile-core" % "2.0.0",
      "io.github.pityka" %% "lamp-umap" % "0.0.115",
    ),
    excludeDependencies ++= Seq(
      ExclusionRule(organization = "ch.qos.logback"),
      ExclusionRule(organization = "org.apache.logging.log4j"),
      ExclusionRule(organization = "commons-logging"),
      ExclusionRule(organization = "log4j")
      
    )
  )
  .settings(
    artifactName in packageSrc in Compile := { (sv: ScalaVersion, module: ModuleID, artifact: Artifact) =>
      "sources.jar"
    },
    resources in Compile += (packageSrc in Compile).value,
    scriptClasspath ++= Seq(
      "../resources/"
    ),
    mappings in Universal ++= (resources in Compile).value.map(file => file -> s"resources/${file.getName}")
  )
  .enablePlugins(GitVersioning)
  .enablePlugins(BuildInfoPlugin)
  .settings(
    buildInfoKeys := Seq[BuildInfoKey](version),
    buildInfoPackage := "buildinfo"
  )
  .enablePlugins(JavaAppPackaging)
  .dependsOn(stat.project)
  .dependsOn(nsplAwt,nsplSaddle,storiesCore)

publishArtifact := false

fork := true

run / javaOptions += "-XX:+UseSerialGC"

lazy val root = (project in file("."))
  .settings(commonSettings: _*)
  .aggregate(core)
