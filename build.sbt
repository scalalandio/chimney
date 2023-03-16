import commandmatrix.extra.*

lazy val isCI = sys.env.get("CI").contains("true")
ThisBuild / scalafmtOnCompile := !isCI

// versions

val versions = new {
  val scala212 = "2.12.17"
  val scala213 = "2.13.10"
  val scala3 = "3.2.2"

  // Which versions should be cross-compiled for publishing
  val scalas = List(scala212, scala213, scala3)
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
  scalacOptions ++= {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((3, _)) =>
        Seq(
          "-explain",
          "-rewrite",
          "-source",
          "3.2-migration",
          "-Ykind-projector:underscores"
        )
      case Some((2, 13)) =>
        Seq(
          // format: off
          "-encoding", "UTF-8",
          "-release", "8",
          // format: on
          "-unchecked",
          "-deprecation",
          "-explaintypes",
          "-feature",
          "-language:higherKinds",
          "-Wunused:patvars",
          "-Xfatal-warnings",
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
          "-Xsource:3",
          "-Ywarn-dead-code",
          "-Ywarn-numeric-widen",
          "-Ywarn-unused:locals",
          "-Ywarn-unused:imports",
          "-Ywarn-macros:after",
          "-Ytasty-reader",
          "-Wconf:origin=scala.collection.compat.*:s",
          "-Wconf:src=io/scalaland/chimney/cats/package.scala:s" // silence package object inheritance deprecation
        )
      case Some((2, 12)) =>
        Seq(
          // format: off
          "-encoding", "UTF-8",
          "-target:jvm-1.8",
          // format: on
          "-unchecked",
          "-deprecation",
          "-explaintypes",
          "-feature",
          "-language:higherKinds",
          "-Xexperimental",
          "-Xfuture",
          "-Xlint:adapted-args",
          "-Xlint:by-name-right-associative",
          "-Xlint:delayedinit-select",
          "-Xlint:doc-detached",
          "-Xlint:inaccessible",
          "-Xlint:infer-any",
          "-Xlint:nullary-override",
          "-Xlint:nullary-unit",
          "-Xlint:option-implicit",
          "-Xlint:package-object-classes",
          "-Xlint:poly-implicit-overload",
          "-Xlint:private-shadow",
          "-Xlint:stars-align",
          "-Xlint:type-parameter-shadow",
          "-Xlint:unsound-match",
          "-Xsource:3",
          "-Yno-adapted-args",
          "-Ywarn-dead-code",
          "-Ywarn-inaccessible",
          "-Ywarn-infer-any",
          "-Ywarn-numeric-widen",
          "-Ywarn-unused:locals",
          "-Ywarn-unused:imports",
          "-Ywarn-macros:after",
          "-Ywarn-nullary-override",
          "-Ywarn-nullary-unit"
        )
      case _ => Seq.empty
    }
  },
  Compile / console / scalacOptions --= Seq("-Ywarn-unused:imports", "-Xfatal-warnings"),
  testFrameworks += new TestFramework("utest.runner.Framework")
)

val dependencies = Seq(
  libraryDependencies ++= Seq(
    "org.scala-lang.modules" %%% "scala-collection-compat" % "2.9.0",
    "com.lihaoyi" %%% "utest" % "0.8.1" % "test"
  ),
  libraryDependencies ++= {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, _)) => Seq(
        "org.scala-lang" % "scala-reflect" % scalaVersion.value % "provided",
        compilerPlugin("org.typelevel" % "kind-projector" % "0.13.2" cross CrossVersion.full)
      )
      case _ => Seq.empty
    }
  },
)

val versionSchemeSettings = Seq(versionScheme := Some("early-semver"))

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

val ciCommand = (platform: String, scalaSuffix: String) => {
  val clean = Seq("clean")
  def withCoverage(tasks: String*): Seq[String] = "coverage" +: tasks :+ "coverageAggregate" :+ "coverageOff"

  val tasks = platform match {
    case "JVM" => // JVM
      clean ++ Seq("scalafmtCheck", "Test/scalafmtCheck") ++
        Seq(s"chimney$scalaSuffix/compile", s"chimneyCats$scalaSuffix/compile") ++
        withCoverage(
          s"chimney$scalaSuffix/test",
          s"chimneyCats$scalaSuffix/test",
          s"chimney$scalaSuffix/coverageReport",
          s"chimneyCats$scalaSuffix/coverageReport"
        ) ++ Seq("benchmarks/compile")
    case "JS" =>
      clean ++ Seq(s"chimneyJS$scalaSuffix/test", s"chimneyCatsJS$scalaSuffix/test")
    case "Native" =>
      clean ++ Seq(s"chimneyNative$scalaSuffix/test", s"chimneyCatsNative$scalaSuffix/test")
  }

  tasks.mkString(";")
}

// modules

lazy val root = project
  .in(file("."))
  .enablePlugins(SphinxPlugin, GhpagesPlugin, GitVersioning, GitBranchPrompt)
  .settings(settings)
  .settings(publishSettings)
  .settings(noPublishSettings)
  .aggregate((chimney.projectRefs ++ chimneyCats.projectRefs)*)
  .settings(
    moduleName := "chimney-build",
    name := "chimney-build",
    description := "Build setup for Chimney modules",
    Sphinx / version := version.value,
    Sphinx / sourceDirectory := file("docs") / "source",
    git.remoteRepo := "git@github.com:scalalandio/chimney.git",
    logo :=
      s"""Chimney ${(ThisBuild / version).value} build for (${versions.scala212}, ${versions.scala213}, ${versions.scala3}) x (Scala JVM, Scala.js $scalaJSVersion, Scala Native $nativeVersion)
         |
         |This build uses sbt-projectmatrix with sbt-commandmatrix helper:
         | - Scala JVM adds no suffix to a project name seen in build.sbt
         | - Scala.js adds the "JS" suffix to a project name seen in build.sbt
         | - Scala Native adds the "Native" suffix to a project name seen in build.sbt
         | - Scala 2.12 adds the suffix "2_12" to a project name seen in build.sbt
         | - Scala 2.13 adds no suffix to a project name seen in build.sbt
         | - Scala 3 adds the suffix "3" to a project name seen in build.sbt
         |
         |When working with IntelliJ, edit "val ideScala = ..." and "val idePlatform = ..." within "val versions" in build.sbt to control which Scala version you're currently working with.""".stripMargin,
    usefulTasks := Seq(
      sbtwelcome
        .UsefulTask(
          "projects",
          "List all projects generated by the build matrix"
        )
        .noAlias,
      sbtwelcome
        .UsefulTask(
          "test",
          "Compile and test all projects in all Scala versions and platforms (beware! it uses a lot of memory and might OOM!)"
        )
        .noAlias,
      sbtwelcome.UsefulTask("publishSigned", "Stage all versions for publishing").noAlias,
      sbtwelcome.UsefulTask("sonatypeBundleRelease", "Publish all artifacts staged for release").noAlias,
      sbtwelcome.UsefulTask("benchmarks/Jmh/run", "Run JMH benchmarks suite").alias("runBenchmarks"),
      sbtwelcome.UsefulTask(ciCommand("JVM", "3"), "CI pipeline for Scala 3 on JVM").alias("ci-jvm-3"),
      sbtwelcome.UsefulTask(ciCommand("JVM", ""), "CI pipeline for Scala 2.13 on JVM").alias("ci-jvm-2_13"),
      sbtwelcome.UsefulTask(ciCommand("JVM", "2_12"), "CI pipeline for Scala 2.12 on JVM").alias("ci-jvm-2_12"),
      sbtwelcome.UsefulTask(ciCommand("JS", "3"), "CI pipeline for Scala 3 on Scala JS").alias("ci-js-3"),
      sbtwelcome.UsefulTask(ciCommand("JS", ""), "CI pipeline for Scala 2.13 on Scala JS").alias("ci-js-2_13"),
      sbtwelcome.UsefulTask(ciCommand("JS", "2_12"), "CI pipeline for Scala 2.12 on Scala JS").alias("ci-js-2_12"),
      sbtwelcome.UsefulTask(ciCommand("Native", "3"), "CI pipeline for Scala 3 on Scala Native").alias("ci-native-3"),
      sbtwelcome
        .UsefulTask(ciCommand("Native", ""), "CI pipeline for Scala 2.13 on Scala Native")
        .alias("ci-native-2_13"),
      sbtwelcome
        .UsefulTask(ciCommand("Native", "2_12"), "CI pipeline for Scala 2.12 on Scala Native")
        .alias("ci-native-2_12")
    )
  )

lazy val chimney = projectMatrix
  .in(file("chimney"))
  .someVariations(versions.scalas, versions.platforms)(only1VersionInIDE*)
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
  .settings(settings*)
  .settings(versionSchemeSettings*)
  .settings(publishSettings*)
  .settings(dependencies*)
  .dependsOn(protos % "test->test")

lazy val chimneyCats = projectMatrix
  .in(file("chimneyCats"))
  .someVariations(versions.scalas, versions.platforms)(only1VersionInIDE*)
  .disablePlugins(WelcomePlugin)
  .settings(
    moduleName := "chimney-cats",
    name := "chimney-cats",
    description := "Integrations with selected Cats data types and type classes"
  )
  .settings(settings*)
  .settings(versionSchemeSettings*)
  .settings(publishSettings*)
  .settings(dependencies*)
  .settings(libraryDependencies += "org.typelevel" %%% "cats-core" % "2.9.0" % "provided")
  .dependsOn(chimney % "test->test;compile->compile")

lazy val protos = projectMatrix
  .in(file("protos"))
  .someVariations(versions.scalas, versions.platforms)(only1VersionInIDE*)
  .disablePlugins(WelcomePlugin)
  .settings(
    moduleName := "chimney-protos",
    name := "chimney-protos",
    description := "Protobufs used for conversion testing"
  )
  .settings(settings*)
  .settings(noPublishSettings*)

lazy val benchmarks = projectMatrix
  .in(file("benchmarks"))
  .someVariations(List(versions.scala213), List(VirtualAxis.jvm))(only1VersionInIDE*) // only makes sense for JVM
  .settings(
    moduleName := "chimney-benchmarks",
    name := "chimney-benchmarks",
    description := "Chimney benchmarking harness"
  )
  .enablePlugins(JmhPlugin)
  .disablePlugins(WelcomePlugin)
  .settings(settings*)
  .settings(noPublishSettings*)
  .dependsOn(chimney)

//when having memory/GC-related errors during build, uncommenting this may be useful:
Global / concurrentRestrictions := Seq(
  Tags.limit(Tags.Compile, 2) // only 2 compilations at once
)
