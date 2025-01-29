val dottyVersion = "3.6.2"

import xerial.sbt.Sonatype.sonatypeCentralHost
import scala.scalanative.build._


ThisBuild/version := "1.0.1-SNAPSHOT"
ThisBuild/versionScheme := Some("semver-spec")
//ThisBuild/resolvers ++= Opts.resolver.sonatypeOssSnapshots
ThisBuild/sonatypeCredentialHost := sonatypeCentralHost
ThisBuild/publishTo := sonatypePublishToBundle.value



val sharedSettings = Seq(
    organization := "io.github.dotty-cps-async",
    scalaVersion := dottyVersion,
    name := "dotty-cps-async-next"
)



lazy val root = project
  .in(file("."))
  .aggregate(cps.js, cps.jvm, cps.native, compilerPlugin, cpsLoomAddOn, logic.jvm, logic.js, logic.native)
  .settings(
    Sphinx / sourceDirectory := baseDirectory.value / "docs",
    SiteScaladocPlugin.scaladocSettings(CpsJVM, cps.jvm / Compile / packageDoc / mappings, "api/jvm"),
    SiteScaladocPlugin.scaladocSettings(CpsJS,  cps.js / Compile / packageDoc / mappings, "api/js"),
    SiteScaladocPlugin.scaladocSettings(CpsNative,  cps.native / Compile / packageDoc / mappings, "api/native"),
    SiteScaladocPlugin.scaladocSettings(Root,  logic.jvm / Compile / packageDoc / mappings, "api/logic/jvm"),
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
        scalacOptions ++= Seq( //"-Yexplicit-nulls",
                            "-unchecked", "-Ydebug-trace", "-Ydebug-names", "-Xprint-types",
                            "-Ydebug", "-uniqid", "-Xcheck-macros", "-Ycheck:macro", "-Yprint-syms",
                            "-Wsafe-init",
                             ),
                             // -explain
                             // -Ydebug-error
                             // -Ydebug-tree-with-id -1
        Compile / doc / scalacOptions := Seq("-groups",  
                "-source-links:shared=github://rssh/dotty-cps-async/master#shared",
                "-source-links:jvm=github://rssh/dotty-cps-async/master#jvm"),
        libraryDependencies += "com.github.sbt" % "junit-interface" % "0.13.3" % "test",
        mimaPreviousArtifacts := Set("com.github.rssh" %% "dotty-cps-async" % "0.9.23")
    ).jsSettings(
        scalaJSUseMainModuleInitializer := true,
        Compile / doc / scalacOptions := Seq("-groups",  
                "-source-links:shared=github://rssh/dotty-cps-async/master#shared",
                "-source-links:js=github://rssh/dotty-cps-async/master#js"),
        libraryDependencies += ("org.scala-js" %% "scalajs-junit-test-runtime" % "1.8.0" % Test).cross(CrossVersion.for3Use2_13),
        mimaFailOnNoPrevious := false
    ).nativeSettings(
        libraryDependencies += "org.scala-native" %%% "junit-runtime" % nativeVersion % Test,
        addCompilerPlugin("org.scala-native" % "junit-plugin" % nativeVersion cross CrossVersion.full),
        //nativeConfig ~= {
        //    _.withSourceLevelDebuggingConfig(_.enableAll) // enable generation of debug informations
        //    .withOptimize(false)  // disable Scala Native optimizer
        //    .withMode(scalanative.build.Mode.debug) // compile using LLVM without optimizations
        //}

    )



lazy val CpsJVM = config("cps.jvm")
lazy val CpsJS = config("cps.js")
lazy val CpsNative = config("cps.native")
lazy val Root = config("root")

lazy val cpsLoomAddOn = project.in(file("jvm-loom-addon"))
  .dependsOn(cps.jvm)
  .disablePlugins(SitePreviewPlugin)
  .settings(sharedSettings)
  .settings(
    name := "dotty-cps-async-loom-next",
    scalacOptions ++= Seq("-Xtarget:21",  "-explain" /*, "-color:never"*/ ),
    libraryDependencies += "com.github.sbt" % "junit-interface" % "0.13.3" % "test",
  )


lazy val cpsLoomTest = project.in(file("jvm-loom-tests"))
                      .dependsOn(cps.jvm, cpsLoomAddOn)
                      .disablePlugins(SitePreviewPlugin)
                      .settings(sharedSettings)
                      .settings(name := "dotty-cps-async-loom-test-next")
                      .settings(
                        // TODO: remove sources, add dependency from java
                        //Compile / unmanagedSourceDirectories ++= Seq(
                        //     baseDirectory.value / ".." / "jvm" / "src" / "main" / "scala",
                        //     baseDirectory.value / ".." / "shared" / "src" / "main" / "scala",
                        //),
                        Compile / unmanagedSourceDirectories := Seq(),
                        Test / unmanagedSourceDirectories ++= Seq(
                             baseDirectory.value / ".." / "jvm" / "src" / "test" / "scala",
                             baseDirectory.value / ".." / "shared" / "src" / "test" / "scala",
                        ),
                        libraryDependencies += "com.github.sbt" % "junit-interface" % "0.13.3" % "test",
                        Test/fork := true,
                        //for macos
                        //Test/javaHome := Some(file("/Library/Java/JavaVirtualMachines/jdk-19.jdk/Contents/Home/")),
                        //for linux:
                        //Test/javaHome := Some(file("/usr/lib/jvm/jdk-19")),

                        //now we have jdk
                        //Test/javaOptions ++= Seq(
                        //   "--enable-preview", 
                        //   "--add-modules", "jdk.incubator.concurrent"
                        //)
                      )




lazy val compilerPlugin = project.in(file("compiler-plugin"))
                           .dependsOn(cps.jvm, cps.js)
                           .settings(sharedSettings)
                           .disablePlugins(SitePreviewPlugin)
                           .settings(
                              name := "dotty-cps-async-compiler-plugin-next",
                              libraryDependencies ++= Seq(
                                  "org.scala-lang" %% "scala3-compiler" % scalaVersion.value % "provided",
                                  "com.github.sbt" % "junit-interface" % "0.13.3" % "test",
                                  ("org.scala-js" %% "scalajs-linker" % "1.18.1").cross(CrossVersion.for3Use2_13) % "test",
                                  ("org.scala-js" %% "scalajs-env-nodejs" % "1.4.0").cross(CrossVersion.for3Use2_13) % "test",
                              ),
                              // TODO: split test into subdirectories.
                              //Test/scalacOptions ++= {
                              //   val jar = (Compile / packageBin).value
                              //   Seq(s"-Xplugin:${jar.getAbsolutePath}", s"-Jdummy=${jar.lastModified}",
                              //        "-color:never",
                              //        "-explain"
                              //      )
                              //},
                              Test/fork := true
                           )

lazy val compilerPluginTests = crossProject(JSPlatform, JVMPlatform, NativePlatform)
                           .in(file("compiler-plugin-tests"))
                           .dependsOn(cps)
                           .jvmConfigure(_.dependsOn(compilerPlugin))
                           .jsConfigure(_.dependsOn(compilerPlugin))
                           .nativeConfigure(_.dependsOn(compilerPlugin))
                           .disablePlugins(SitePreviewPlugin)
                           .settings(sharedSettings)
                           .settings(
                              name := "dotty-cps-compiler-plugin-tests-next",
                              libraryDependencies ++= Seq(
                                  "org.scala-lang" %% "scala3-compiler" % scalaVersion.value % "provided",
                                  "com.github.sbt" % "junit-interface" % "0.13.3" % "test",
                              ),
                              Compile / unmanagedSourceDirectories := Seq(),
                              Test / unmanagedSourceDirectories ++= Seq(
                               baseDirectory.value / ".." / ".." / "shared" / "src" / "test" / "scala",
                              ),
                              Test / scalacOptions ++= {
                                  val jar = (compilerPlugin / Compile / packageBin).value
                                  Seq(s"-Xplugin:${jar.getAbsolutePath}", s"-Jdummy=${jar.lastModified}",
                                       "-color:never",
                                       "-explain",
                                     )
                              }
                           ).jvmSettings(
                              libraryDependencies += "com.github.sbt" % "junit-interface" % "0.13.3" % "test",
                              Test / unmanagedSourceDirectories ++= Seq(
                                baseDirectory.value / ".." / ".." / "jvm" / "src" / "test" / "scala",
                              ),
                              Test / unmanagedSources / excludeFilter := "TestSF1W1.scala" || "TestSL3.scala" || "TestSF4.scala"
                           ).jsSettings(
                              scalaJSUseMainModuleInitializer := true,
                              libraryDependencies += ("org.scala-js" %% "scalajs-junit-test-runtime" % "1.8.0" % Test).cross(CrossVersion.for3Use2_13),
                              mimaFailOnNoPrevious := false,
                              Test / unmanagedSourceDirectories ++= Seq(
                                  baseDirectory.value / ".." / ".." / "js" / "src" / "test" / "scala",
                              ),
                              Test / unmanagedSources / excludeFilter := "TestSF1W1.scala" || "TestSL3.scala" || "TestSF4.scala"
                           ).nativeSettings(
                              libraryDependencies += "org.scala-native" %%% "junit-runtime" % nativeVersion % Test,
                              addCompilerPlugin("org.scala-native" % "junit-plugin" % nativeVersion cross CrossVersion.full),
                              Test / unmanagedSourceDirectories ++= Seq(
                                  baseDirectory.value / ".." / ".." / "native" / "src" / "test" / "scala"
                              ),
                              Test / unmanagedSources / excludeFilter := "TestSF1W1.scala" || "TestSL3.scala" || "TestSF4.scala"
                           )

lazy val logic = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("logic"))
  .dependsOn(cps)
  .settings(sharedSettings)
  .disablePlugins(SitePreviewPlugin)
  .settings(
    name := "dotty-cps-async-logic-next",
    libraryDependencies += "com.github.sbt" % "junit-interface" % "0.13.3" % "test",
  ).jsSettings(
    scalaJSUseMainModuleInitializer := true,
    libraryDependencies += ("org.scala-js" %% "scalajs-junit-test-runtime" % "1.8.0" % Test).cross(CrossVersion.for3Use2_13),
  ).nativeSettings(
    libraryDependencies += "org.scala-native" %%% "junit-runtime" % nativeVersion % Test,
    addCompilerPlugin("org.scala-native" % "junit-plugin" % nativeVersion cross CrossVersion.full)
  )


