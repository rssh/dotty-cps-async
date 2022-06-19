//val dottyVersion = "3.0.2-RC1-bin-SNAPSHOT"
//val dottyVersion = "3.1.2-RC1-bin-SNAPSHOT"
val dottyVersion = "3.1.3"
//val dottyVersion = "3.1.1"

ThisBuild/version := "0.9.10-SNAPSHOT"
ThisBuild/versionScheme := Some("semver-spec")
ThisBuild/resolvers += Opts.resolver.sonatypeSnapshots



val sharedSettings = Seq(
    organization := "com.github.rssh",
    scalaVersion := dottyVersion,
    name := "dotty-cps-async",
)
  

lazy val root = project
  .in(file("."))
  .aggregate(cps.js, cps.jvm, cps.native)
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


lazy val cps = crossProject(JSPlatform, JVMPlatform, NativePlatform)
    .in(file("."))
    .settings(sharedSettings)
    .disablePlugins(SitePreviewPlugin)
    .jvmSettings(
        scalacOptions ++= Seq( "-Yexplicit-nulls",
                            "-unchecked", "-Ydebug-trace", "-Ydebug-names", "-Xprint-types", 
                            "-Ydebug", "-uniqid", "-Xcheck-macros", "-Ycheck:macro", "-Yprint-syms", "-explain"  ),
                             // -explain
                             // -Ydebug-error
                             // -Ydebug-tree-with-id -1
        Compile / doc / scalacOptions := Seq("-groups",  
                "-source-links:shared=github://rssh/dotty-cps-async/master#shared",
                "-source-links:jvm=github://rssh/dotty-cps-async/master#jvm"),
        libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test",
        mimaPreviousArtifacts := Set("com.github.rssh" %% "dotty-cps-async" % "0.9.8")
    ).jsSettings(
        scalaJSUseMainModuleInitializer := true,
        Compile / doc / scalacOptions := Seq("-groups",  
                "-source-links:shared=github://rssh/dotty-cps-async/master#shared",
                "-source-links:js=github://rssh/dotty-cps-async/master#js"),
        libraryDependencies += ("org.scala-js" %% "scalajs-junit-test-runtime" % "1.8.0" % Test).cross(CrossVersion.for3Use2_13),
        mimaFailOnNoPrevious := false
    ).nativeSettings(
        scalaVersion := "3.1.2",
        libraryDependencies += "org.scala-native" %%% "junit-runtime" % nativeVersion % Test,
        libraryDependencies += "com.github.lolgab" %%% "native-loop-core" % "0.2.1" % Test,
        addCompilerPlugin("org.scala-native" % "junit-plugin" % nativeVersion cross CrossVersion.full) 

    )

lazy val CpsJVM = config("cps.jvm")
lazy val CpsJS = config("cps.js")
//lazy val CpsNative = config("cps.native")
lazy val Root = config("root")
