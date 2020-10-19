//val dottyVersion = "0.27.0-RC1"
//val dottyVersion = "0.28.0-bin-20200907-101e620-NIGHTLY"
//val dottyVersion = "0.28.0-bin-SNAPSHOT"
//val dottyVersion = "3.0.0-M1-bin-20201017-c76800d-NIGHTLY"
val dottyVersion = dottyLatestNightlyBuild.get


val sharedSettings = Seq(
    version := "0.3.0-SNAPSHOT",
    organization := "com.github.rssh",
    scalaVersion := dottyVersion,
    name := "dotty-cps-async"
)

lazy val root = project
  .in(file("."))
  .aggregate(cps.js, cps.jvm)
  .settings(
    Sphinx / sourceDirectory := baseDirectory.value / "docs",
    git.remoteRepo := "git@github.com:rssh/dotty-cps-async.git"
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

