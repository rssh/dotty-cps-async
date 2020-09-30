//val dottyVersion = "0.27.0-RC1"
//val dottyVersion = "0.28.0-bin-20200907-101e620-NIGHTLY"
//val dottyVersion = "0.28.0-bin-SNAPSHOT"
val dottyVersion = dottyLatestNightlyBuild.get

enablePlugins(SphinxPlugin)
enablePlugins(GhpagesPlugin)

git.remoteRepo := "git@github.com:rssh/dotty-cps-async.git"

val sharedSettings = Seq(
    version := "0.3.0-SNAPSHOT",
    organization := "com.github.rssh",
    scalaVersion := dottyVersion,
)

lazy val root = project
  .in(file("."))
  .aggregate(cps.js, cps.jvm)
  .settings(
    name := "dotty-cps-async",
    Sphinx / sourceDirectory := baseDirectory.value / "docs",
  )


lazy val cps = crossProject(JSPlatform, JVMPlatform)
    .in(file("."))
    .settings(sharedSettings)
    .jvmSettings(
        scalacOptions ++= Seq( "-unchecked", "-Ydebug-trace", "-Ydebug-names", "-Xprint-types", 
                            "-Ydebug-type-error", "-uniqid" ),
        libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test",
    ).jsSettings(
        scalaJSUseMainModuleInitializer := true,
        libraryDependencies += ("org.scala-js" %% "scalajs-junit-test-runtime" % "1.2.0" % Test).withDottyCompat(scalaVersion.value)
    )

