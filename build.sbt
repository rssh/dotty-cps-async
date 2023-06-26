//val dottyVersion = "3.3.2-RC1-bin-SNAPSHOT"
val dottyVersion = "3.3.0"


ThisBuild/version := "0.9.18-SNAPSHOT"
ThisBuild/versionScheme := Some("semver-spec")
ThisBuild/resolvers ++= Opts.resolver.sonatypeOssSnapshots



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
        scalacOptions ++= Seq( //"-Yexplicit-nulls",
                            "-unchecked", "-Ydebug-trace", "-Ydebug-names", "-Xprint-types",
                            "-Ydebug", "-uniqid", "-Xcheck-macros", "-Ycheck:macro", "-Yprint-syms",
                            "-Ysafe-init",
                            "-explain",
                             ),
                             // -explain
                             // -Ydebug-error
                             // -Ydebug-tree-with-id -1
        Compile / doc / scalacOptions := Seq("-groups",  
                "-source-links:shared=github://rssh/dotty-cps-async/master#shared",
                "-source-links:jvm=github://rssh/dotty-cps-async/master#jvm"),
        libraryDependencies += "com.github.sbt" % "junit-interface" % "0.13.3" % "test",
        mimaPreviousArtifacts := Set("com.github.rssh" %% "dotty-cps-async" % "0.9.9")
    ).jsSettings(
        scalaJSUseMainModuleInitializer := true,
        Compile / doc / scalacOptions := Seq("-groups",  
                "-source-links:shared=github://rssh/dotty-cps-async/master#shared",
                "-source-links:js=github://rssh/dotty-cps-async/master#js"),
        libraryDependencies += ("org.scala-js" %% "scalajs-junit-test-runtime" % "1.8.0" % Test).cross(CrossVersion.for3Use2_13),
        mimaFailOnNoPrevious := false
    ).nativeSettings(
        //scalaVersion := "3.1.2",
        libraryDependencies += "org.scala-native" %%% "junit-runtime" % nativeVersion % Test,
        libraryDependencies += "com.github.lolgab" %%% "native-loop-core" % "0.2.1" % Test,
        addCompilerPlugin("org.scala-native" % "junit-plugin" % nativeVersion cross CrossVersion.full)
    )

lazy val CpsJVM = config("cps.jvm")
lazy val CpsJS = config("cps.js")
//lazy val CpsNative = config("cps.native")
lazy val Root = config("root")


lazy val cpsLoomJVM = project.in(file("jvm-loom"))
                      .dependsOn(cps.jvm)
                      .settings(sharedSettings)
                      .settings(name := "dotty-cps-async-loom-test")
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
                        libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test",
                        Test/fork := true,
                        //for macos
                        Test/javaHome := Some(file("/Library/Java/JavaVirtualMachines/jdk-19.jdk/Contents/Home/")),
                        //for linux:
                        //Test/javaHome := Some(file("/usr/lib/jvm/jdk-19")),

                        Test/javaOptions ++= Seq(
                           "--enable-preview", 
                           "--add-modules", "jdk.incubator.concurrent"
                        )
                      )


lazy val compilerPlugin = project.in(file("compiler-plugin"))
                           .dependsOn(cps.jvm)
                           .settings(sharedSettings)
                           .settings(
                              name := "dotty-cps-compiler-plugin",
                              libraryDependencies ++= Seq(
                                  "org.scala-lang" %% "scala3-compiler" % scalaVersion.value % "provided",
                                  "com.novocode" % "junit-interface" % "0.11" % Test,
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
                           .settings(sharedSettings)
                           .settings(
                              name := "dotty-cps-compiler-plugin-tests",
                              libraryDependencies ++= Seq(
                                  "org.scala-lang" %% "scala3-compiler" % scalaVersion.value % "provided",
                                  "com.novocode" % "junit-interface" % "0.11" % Test,
                              ),
                              Compile / unmanagedSourceDirectories := Seq(),
                              Test / unmanagedSourceDirectories ++= Seq(
                               baseDirectory.value / ".." / ".." / "shared" / "src" / "test" / "scala",
                              ),
                              Test / scalacOptions ++= {
                                  val jar = (compilerPlugin / Compile / packageBin).value
                                  Seq(s"-Xplugin:${jar.getAbsolutePath}", s"-Jdummy=${jar.lastModified}",
                                       "-color:never",
                                       "-explain"
                                     )
                              }
                           ).jvmSettings(
                              libraryDependencies += "com.github.sbt" % "junit-interface" % "0.13.3" % "test",
                              Test / unmanagedSourceDirectories ++= Seq(
                                baseDirectory.value / ".." / ".." / "jvm" / "src" / "test" / "scala",
                              )
                           ).jsSettings(
                              scalaJSUseMainModuleInitializer := true,
                              libraryDependencies += ("org.scala-js" %% "scalajs-junit-test-runtime" % "1.8.0" % Test).cross(CrossVersion.for3Use2_13),
                              mimaFailOnNoPrevious := false,
                              Test / unmanagedSourceDirectories ++= Seq(
                                  baseDirectory.value / ".." / "jvm" / "src" / "test" / "scala",
                              )
                           ).nativeSettings(
                              libraryDependencies += "org.scala-native" %%% "junit-runtime" % nativeVersion % Test,
                              libraryDependencies += "com.github.lolgab" %%% "native-loop-core" % "0.2.1" % Test,
                              addCompilerPlugin("org.scala-native" % "junit-plugin" % nativeVersion cross CrossVersion.full),
                              Test / unmanagedSourceDirectories ++= Seq(
                                  baseDirectory.value / ".." / "native" / "src" / "test" / "scala"
                              )
                           )



