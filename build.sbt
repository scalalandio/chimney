import commandmatrix.extra._

lazy val isCI = sys.env.get("CI").contains("true")
ThisBuild / scalafmtOnCompile := !isCI

// versions

ThisProject / versionScheme := Some("early-semver")

val versions = new {
  val scala212 = "2.12.17"
  val scala213 = "2.13.10"

  // Which versions should be cross-compiled for publishing
  val scalas = List(scala212, scala213)
  val platforms = List(VirtualAxis.jvm, VirtualAxis.js, VirtualAxis.native)

  // Which version should be used in IntelliJ
  val ideScala = scala213
  val idePlatform = VirtualAxis.jvm
}

// common settings

Global / excludeLintKeys += ideSkipProject
val only1VersionInIDE =
  MatrixAction
    .ForPlatform(versions.idePlatform)
    .Configure(_.settings(ideSkipProject := (scalaVersion.value != versions.ideScala))) +:
    versions.platforms.filter(_ != versions.idePlatform).map { platform =>
      MatrixAction
        .ForPlatform(platform)
        .Configure(_.settings(ideSkipProject := true))
    }

val settings = Seq(
  git.useGitDescribe := true,
  git.uncommittedSignifier := None,
  scalacOptions ++= Seq(
    "-encoding",
    "UTF-8",
    "-unchecked",
    "-deprecation",
    "-explaintypes",
    "-feature",
    "-Ywarn-dead-code",
    "-Ywarn-numeric-widen",
    "-Xlint:adapted-args",
    "-Xlint:delayedinit-select",
    "-Xlint:doc-detached",
    "-Xlint:inaccessible",
    "-Xlint:infer-any",
    "-Xlint:nullary-unit",
    "-Xlint:option-implicit",
    "-Xlint:package-object-classes",
    "-Xlint:poly-implicit-overload",
    "-Xlint:private-shadow",
    "-Xlint:stars-align",
    "-Xlint:type-parameter-shadow",
    "-Ywarn-unused:locals",
    "-Ywarn-unused:imports",
    "-Ywarn-macros:after",
//    "-Xfatal-warnings",
    "-language:higherKinds"
  ),
  scalacOptions ++= (
    if (scalaVersion.value >= "2.13")
      Seq(
        "-release",
        "8",
        "-Wunused:patvars"
      )
    else
      Seq(
        "-target:jvm-1.8",
        "-Xfuture",
        "-Xexperimental",
        "-Yno-adapted-args",
        "-Ywarn-inaccessible",
        "-Ywarn-infer-any",
        "-Ywarn-nullary-override",
        "-Ywarn-nullary-unit",
        "-Xlint:by-name-right-associative",
        "-Xlint:unsound-match",
        "-Xlint:nullary-override"
      )
  ),
  Compile / console / scalacOptions --= Seq("-Ywarn-unused:imports", "-Xfatal-warnings"),
  testFrameworks += new TestFramework("utest.runner.Framework")
)

val dependencies = Seq(
  libraryDependencies ++= Seq(
    "org.scala-lang.modules" %%% "scala-collection-compat" % "2.8.0",
    "org.scala-lang" % "scala-reflect" % scalaVersion.value % "provided",
    "com.lihaoyi" %%% "utest" % "0.8.0" % "test"
  ),
  addCompilerPlugin("org.typelevel" % "kind-projector" % "0.13.2" cross CrossVersion.full)
)

val publishSettings = Seq(
  organization := "io.scalaland",
  homepage := Some(url("https://scalaland.io")),
  licenses := Seq("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
  scmInfo := Some(
    ScmInfo(url("https://github.com/scalalandio/chimney"), "scm:git:git@github.com:scalalandio/chimney.git")
  ),
  publishTo := sonatypePublishToBundle.value,
  publishMavenStyle := true,
  Test / publishArtifact := false,
  pomIncludeRepository := { _ =>
    false
  },
  pomExtra := (
    <developers>
      <developer>
        <id>krzemin</id>
        <name>Piotr Krzemiński</name>
        <url>http://github.com/krzemin</url>
      </developer>
      <developer>
        <id>MateuszKubuszok</id>
        <name>Mateusz Kubuszok</name>
        <url>http://github.com/MateuszKubuszok</url>
      </developer>
    </developers>
  )
)

val noPublishSettings =
  Seq(publish / skip := true, publishArtifact := false)

val ciCommand = (scalaSuffix: String) =>
  Iterable(
    "clean",
    "scalafmtCheck",
    "Test/scalafmtCheck",
    s"chimneyCats$scalaSuffix/compile",
    s"chimneyCatsJS$scalaSuffix/compile",
    s"chimneyCatsNative$scalaSuffix/compile",
    "coverage",
    s"chimney$scalaSuffix/test",
    s"chimneyCats$scalaSuffix/test",
    s"chimney$scalaSuffix/coverageReport",
    s"chimneyCats$scalaSuffix/coverageReport",
    "coverageAggregate",
    "coverageOff",
    s"chimneyJS$scalaSuffix/test",
    s"chimneyCatsJS$scalaSuffix/test",
    s"chimneyNative$scalaSuffix/test",
    s"chimneyCatsNative$scalaSuffix/test",
    "benchmarks/compile"
  ).mkString(";")

// modules

lazy val root = project
  .in(file("."))
  .enablePlugins(SphinxPlugin, GhpagesPlugin, GitVersioning, GitBranchPrompt)
  .settings(settings: _*)
  .settings(publishSettings: _*)
  .settings(noPublishSettings: _*)
  .aggregate(chimney.projectRefs ++ chimneyCats.projectRefs: _*)
  .settings(
    moduleName := "chimney-build",
    name := "chimney-build",
    description := "Build setup for Chimney modules",
    Sphinx / version := version.value,
    Sphinx / sourceDirectory := file("docs") / "source",
    git.remoteRepo := "git@github.com:scalalandio/chimney.git",
    logo :=
      s"""Chimney ${(ThisBuild / version).value} build for (${versions.scala212}, ${versions.scala213}) x (Scala JVM, Scala.js $scalaJSVersion, Scala Native $nativeVersion)
         |
         |This build uses sbt-projectmatrix with sbt-commandmatrix helper:
         | - Scala JVM adds no suffix to a project name seen in build.sbt
         | - Scala.js adds the "JS" suffix to a project name seen in build.sbt
         | - Scala Native adds the "Native" suffix to a project name seen in build.sbt
         | - Scala 2.12 adds the suffix "2_12" to a project name seen in build.sbt
         | - Scala 2.13 adds no suffix to a project name seen in build.sbt
         |
         |When working with IntelliJ, edit "val ideScala = ..." and "val idePlatform = ..." within "val versions" in build.sbt to control which Scala version you're currently working with.""".stripMargin,
    usefulTasks := Seq(
      sbtwelcome.UsefulTask("listAll", "projects", "List all projects generated by the build matrix"),
      sbtwelcome.UsefulTask(
        "testAll",
        "test",
        "Compile and test all projects in all Scala versions and platforms (beware! it uses a lot of memory and might OOM!)"
      ),
      sbtwelcome.UsefulTask("stageRelease", "publishSigned", "Stage all versions for publishing"),
      sbtwelcome.UsefulTask("publishRelease", "sonatypeBundleRelease", "Publish all artifacts staged for release"),
      sbtwelcome.UsefulTask(
        "ci-2_12",
        ciCommand("2_12"),
        "Checks formatting, run tests and compute core library coverage for Scala 2.12"
      ),
      sbtwelcome.UsefulTask(
        "ci-2_13",
        ciCommand(""),
        "Checks formatting, run tests and compute core library coverage for Scala 2.13"
      ),
      sbtwelcome.UsefulTask(
        "runBenchmarks",
        "benchmarks/Jmh/run",
        "Run JMH benchmarks suite"
      )
    )
  )

lazy val chimney = projectMatrix
  .in(file("chimney"))
  .someVariations(versions.scalas, versions.platforms)(only1VersionInIDE: _*)
  .disablePlugins(WelcomePlugin)
  .settings(
    moduleName := "chimney",
    name := "chimney",
    description := "Scala library for boilerplate-free data rewriting",
    Compile / doc / scalacOptions ++= {
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, _)) => Seq("-skip-packages", "io.scalaland.chimney.internal")
        case _            => Seq.empty
      }
    }
  )
  .settings(settings: _*)
  .settings(publishSettings: _*)
  .settings(dependencies: _*)
  .dependsOn(protos % "test->test")

lazy val chimneyCats = projectMatrix
  .in(file("chimneyCats"))
  .someVariations(versions.scalas, versions.platforms)(only1VersionInIDE: _*)
  .disablePlugins(WelcomePlugin)
  .settings(
    moduleName := "chimney-cats",
    name := "chimney-cats",
    description := "Integrations with selected Cats data types and type classes"
  )
  .settings(settings: _*)
  .settings(publishSettings: _*)
  .settings(dependencies: _*)
  .settings(libraryDependencies += "org.typelevel" %%% "cats-core" % "2.8.0" % "provided")
  .dependsOn(chimney % "test->test;compile->compile")

lazy val protos = projectMatrix
  .in(file("protos"))
  .someVariations(versions.scalas, versions.platforms)(only1VersionInIDE: _*)
  .disablePlugins(WelcomePlugin)
  .settings(
    moduleName := "chimney-protos",
    name := "chimney-protos",
    description := "Protobufs used for conversion testing"
  )
  .settings(settings: _*)
  .settings(noPublishSettings: _*)

lazy val benchmarks = projectMatrix
  .in(file("benchmarks"))
  .someVariations(versions.scalas, List(VirtualAxis.jvm))(only1VersionInIDE: _*) // only makes sense for JVM
  .settings(
    moduleName := "chimney-benchmarks",
    name := "chimney-benchmarks",
    description := "Chimney benchmarking harness"
  )
  .enablePlugins(JmhPlugin)
  .disablePlugins(WelcomePlugin)
  .settings(settings: _*)
  .settings(noPublishSettings: _*)
  .dependsOn(chimney)

//when having memory/GC-related errors during build, uncommenting this may be useful:
Global / concurrentRestrictions := Seq(
  Tags.limit(Tags.Compile, 2) // only 2 compilations at once
)
