val dottyVersion = "3.0.1-RC1-bin-SNAPSHOT"
//val dottyVersion = "3.0.0-RC3"
//val dottyVersion = dottyLatestNightlyBuild.get

ThisBuild/version := "0.7.0-SNAPSHOT"
ThisBuild/versionScheme := Some("semver-spec")

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
                            "-Ydebug", "-uniqid", "-Ycheck:macros", "-Yprint-syms"  ),
                             // -Ydebug-error
        libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test",
    ).jsSettings(
        scalaJSUseMainModuleInitializer := true,
        libraryDependencies += ("org.scala-js" %% "scalajs-junit-test-runtime" % "1.5.1" % Test).cross(CrossVersion.for3Use2_13)
    )

