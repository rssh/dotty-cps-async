//val dottyVersion = "3.0.0-M1-bin-20201022-b26dbc4-NIGHTLY"
//val dottyVersion = "3.0.0-M3-bin-SNAPSHOT"
val dottyVersion = dottyLatestNightlyBuild.get

ThisBuild/version := "0.3.5-SNAPSHOT"

val sharedSettings = Seq(
    organization := "com.github.rssh",
    scalaVersion := dottyVersion,
    name := "dotty-cps-async"
)

lazy val root = project
  .in(file("."))
  .aggregate(cps.js, cps.jvm)
  .settings(
    Sphinx / sourceDirectory := baseDirectory.value / "docs",
    git.remoteRepo := "git@github.com:rssh/dotty-cps-async.git",
    publishArtifact := false,
  ).enablePlugins(SphinxPlugin)
   .enablePlugins(GhpagesPlugin)


lazy val cps = crossProject(JSPlatform, JVMPlatform)
    .in(file("."))
    .settings(sharedSettings)
    .disablePlugins(SitePlugin)
    .jvmSettings(
        scalacOptions ++= Seq( "-unchecked", "-Ydebug-trace", "-Ydebug-names", "-Xprint-types", 
                            "-Ydebug-type-error", "-uniqid" ),
        libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test",
    ).jsSettings(
        scalaJSUseMainModuleInitializer := true,
        libraryDependencies += ("org.scala-js" %% "scalajs-junit-test-runtime" % "1.2.0" % Test).withDottyCompat(scalaVersion.value)
    )

