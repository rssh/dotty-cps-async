//val dottyVersion = "0.21.0-RC1"
val dottyVersion = dottyLatestNightlyBuild.get

lazy val root = project
  .in(file("."))
  .settings(
    name := "dotty-cps",
    version := "0.1.0",

    scalaVersion := dottyVersion,
    scalacOptions ++= Seq( "-Ydebug", "-Ydebug-trace", "-Ydebug-names"),

    libraryDependencies += "ch.epfl.lamp" %% "dotty-staging" % dottyVersion,
    libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test"
  )
