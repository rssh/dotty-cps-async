//val dottyVersion = "0.26.0-RC1"
val dottyVersion = dottyLatestNightlyBuild.get

enablePlugins(SphinxPlugin)


lazy val root = project
  .in(file("."))
  .settings(
    name := "dotty-cps",
    version := "0.1.0",
    scalaVersion := dottyVersion,

    //scalacOptions ++= Seq( "-Ydebug:implicits", "-Ydebug-trace", "-Ydebug-names", "-Ylog:typer", "-Yplain-printer" ),
    scalacOptions ++= Seq( "-unchecked", "-Ydebug-trace", "-Ydebug-names", "-explain-types", "-Xprint-types", "-Ydebug-type-error"  ),

    //libraryDependencies += "ch.epfl.lamp" %% "dotty-staging" % dottyVersion,
    libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test",

    Sphinx / sourceDirectory := baseDirectory.value / "docs"

  )


