//val dottyVersion = "0.27.0-RC1"
val dottyVersion = "0.28.0-bin-20200907-101e620-NIGHTLY"
//val dottyVersion = dottyLatestNightlyBuild.get

enablePlugins(SphinxPlugin)
enablePlugins(GhpagesPlugin)

git.remoteRepo := "git@github.com:rssh/dotty-cps-async.git"

lazy val root = project
  .in(file("."))
  .settings(
    name := "dotty-cps-async",
    version := "0.3.0-SNAPSHOT",
    organization := "com.github.rssh",
    scalaVersion := dottyVersion,

    //scalacOptions ++= Seq( "-Ydebug:implicits", "-Ydebug-trace", "-Ydebug-names", "-Ylog:typer", "-Yplain-printer" ),
    scalacOptions ++= Seq( "-unchecked", "-Ydebug-trace", "-Ydebug-names", "-Xprint-types", "-Ydebug-type-error", "-uniqid"  ),
        // -explain-types

    //libraryDependencies += "ch.epfl.lamp" %% "dotty-staging" % dottyVersion,
    libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test",

    Sphinx / sourceDirectory := baseDirectory.value / "docs",

  )


