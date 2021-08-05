//val dottyVersion = "3.0.2-RC1-bin-SNAPSHOT"
//val dottyVersion = "3.0.1-RC2"
val dottyVersion = "3.0.1"

ThisBuild/version := "0.9.2"
ThisBuild/versionScheme := Some("semver-spec")


val sharedSettings = Seq(
    organization := "com.github.rssh",
    scalaVersion := dottyVersion,
    name := "dotty-cps-async",
)
  

lazy val root = project
  .in(file("."))
  .aggregate(cps.js, cps.jvm)
  .settings(
    Sphinx / sourceDirectory := baseDirectory.value / "docs",
    SiteScaladocPlugin.scaladocSettings(CpsJVM, cps.jvm / Compile / packageDoc / mappings, "api/jvm"),
    SiteScaladocPlugin.scaladocSettings(CpsJS,  cps.js / Compile / packageDoc / mappings, "api/js"),
    siteDirectory :=  baseDirectory.value / "target" / "site",    
    git.remoteRepo := "git@github.com:rssh/dotty-cps-async.git",
    publishArtifact := false,
  )
  .enablePlugins(SphinxPlugin,
                 SiteScaladocPlugin,
                 GhpagesPlugin
  ).disablePlugins(MimaPlugin)


lazy val cps = crossProject(JSPlatform, JVMPlatform)
    .in(file("."))
    .settings(sharedSettings)
    .disablePlugins(SitePreviewPlugin)
    .jvmSettings(
        scalacOptions ++= Seq( "-unchecked", "-Ydebug-trace", "-Ydebug-names", "-Xprint-types", 
                            "-Ydebug", "-uniqid", "-Ycheck:macros", "-Yprint-syms"  ),
                             // -Ydebug-error
        Compile / doc / scalacOptions := Seq("-groups",  
                "-source-links:shared=github://rssh/dotty-cps-async/master#shared",
                "-source-links:jvm=github://rssh/dotty-cps-async/master#jvm"),
        libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test",
        mimaPreviousArtifacts := Set("com.github.rssh" %% "dotty-cps-async" % "0.9.1")
    ).jsSettings(
        scalaJSUseMainModuleInitializer := true,
        Compile / doc / scalacOptions := Seq("-groups",  
                "-source-links:shared=github://rssh/dotty-cps-async/master#shared",
                "-source-links:js=github://rssh/dotty-cps-async/master#js"),
        libraryDependencies += ("org.scala-js" %% "scalajs-junit-test-runtime" % "1.7.0" % Test).cross(CrossVersion.for3Use2_13),
        mimaFailOnNoPrevious := false
    )

lazy val CpsJVM = config("cps.jvm")
lazy val CpsJS = config("cps.js")
lazy val Root = config("root")
