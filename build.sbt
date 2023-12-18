import com.jsuereth.sbtpgp.PgpKeys.publishSigned
import com.typesafe.tools.mima.core.{Problem, ProblemFilters}
import commandmatrix.extra.*

lazy val isCI = sys.env.get("CI").contains("true")
ThisBuild / scalafmtOnCompile := !isCI

// publishSigned emits
//   Could not find any member to link for "scala.None".
// and similar, and we cannot turn them off in any other way than by removing -Xfatal-warnings
// so at least we can remove them ONLY when building a release from a commit which already
// passed the tests.
lazy val isRelease = sys.env.get("RELEASE").contains("true")

lazy val ciRelease = taskKey[Unit](
  "Publish artifacts to release or snapshot (skipping sonatypeBundleRelease which is unnecessary for snapshots)"
)
ciRelease := {
  publishSigned.taskValue
  Def.taskIf {
    if (git.gitCurrentTags.value.nonEmpty) {
      sonatypeBundleRelease.taskValue
    }
  }
}

// versions

val versions = new {
  val scala212 = "2.12.18"
  val scala213 = "2.13.12"
  val scala3 = "3.3.1"

  // Which versions should be cross-compiled for publishing
  val scalas = List(scala212, scala213, scala3)
  val platforms = List(VirtualAxis.jvm, VirtualAxis.js, VirtualAxis.native)

  // Which version should be used in IntelliJ
  val ideScala = scala3
  val idePlatform = VirtualAxis.jvm
}

// common settings

Global / excludeLintKeys += git.useGitDescribe
Global / excludeLintKeys += ideSkipProject
Global / excludeLintKeys += sourceDirectories
val only1VersionInIDE =
  MatrixAction
    .ForPlatform(versions.idePlatform)
    .Configure(
      _.settings(
        ideSkipProject := (scalaVersion.value != versions.ideScala),
        bspEnabled := (scalaVersion.value == versions.ideScala),
        scalafmtOnCompile := !isCI
      )
    ) +:
    versions.platforms.filter(_ != versions.idePlatform).map { platform =>
      MatrixAction
        .ForPlatform(platform)
        .Configure(_.settings(ideSkipProject := true, bspEnabled := false, scalafmtOnCompile := false))
    }

val non212tests =
  MatrixAction
    .ForScala(v => (v.value == versions.scala213) || v.isScala3)
    .Configure(
      _.settings(
        // Compile / unmanagedSourceDirectories += sourceDirectory.value.toPath.resolve("./main/scala-2.13+").toFile,
        Test / unmanagedSourceDirectories += sourceDirectory.value.toPath.resolve("test/scala-2.13+").toFile
      )
    )

val settings = Seq(
  git.useGitDescribe := true,
  git.uncommittedSignifier := None,
  scalacOptions ++= {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((3, _)) =>
        Seq(
          // format: off
          "-encoding", "UTF-8",
          "-rewrite",
          "-source", "3.3-migration",
          // format: on
          "-unchecked",
          "-deprecation",
          "-explain",
          "-explain-types",
          "-feature",
          "-Wconf:msg=Unreachable case:s", // suppress fake (?) errors in internal.compiletime
          "-Wnonunit-statement",
          // "-Wunused:imports", // import x.Underlying as X is marked as unused even though it is!
          "-Wunused:privates",
          "-Wunused:locals",
          "-Wunused:explicits",
          "-Wunused:implicits",
          "-Wunused:params",
          "-Wvalue-discard",
          "-Xfatal-warnings",
          "-Xcheck-macros",
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
          "-Wconf:origin=scala.collection.compat.*:s", // type aliases without which 2.12 fail compilation but 2.13 doesn't need them
          "-Wconf:cat=scala3-migration:s", // silence mainly issues with -Xsource:3 and private case class constructors
          "-Wconf:cat=deprecation&origin=io.scalaland.chimney.*:s", // we want to be able to deprecate APIs and test them while they're deprecated
          "-Wconf:msg=The outer reference in this type test cannot be checked at run time:s", // suppress fake(?) errors in internal.compiletime (adding origin breaks this suppression)
          "-Wconf:src=io/scalaland/chimney/cats/package.scala:s", // silence package object inheritance deprecation
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
          "-Ytasty-reader"
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
          "-Wconf:cat=deprecation&origin=io.scalaland.chimney.*:s", // we want to be able to deprecate APIs and test them while they're deprecated
          "-Wconf:msg=The outer reference in this type test cannot be checked at run time:s", // suppress fake(?) errors in internal.compiletime (adding origin breaks this suppression)
          "-Wconf:src=io/scalaland/chimney/cats/package.scala:s", // silence package object inheritance deprecation
          "-Xexperimental",
          "-Xfatal-warnings",
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
  Compile / doc / scalacOptions ++= {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((3, _)) =>
        Seq("-Ygenerate-inkuire") // type-based search for Scala 3, this option cannot go into compile
      case _ => Seq.empty
    }
  },
  Compile / console / scalacOptions --= Seq("-Ywarn-unused:imports", "-Xfatal-warnings"),
  Test / compile / scalacOptions --= {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, 12)) => Seq("-Ywarn-unused:locals") // Scala 2.12 ignores @unused warns
      case _             => Seq.empty
    }
  }
)

val dependencies = Seq(
  libraryDependencies ++= Seq(
    "org.scala-lang.modules" %%% "scala-collection-compat" % "2.11.0",
    "org.scalameta" %%% "munit" % "1.0.0-M10" % "test"
  ),
  libraryDependencies ++= {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, _)) =>
        Seq(
          "org.scala-lang" % "scala-reflect" % scalaVersion.value % "provided",
          compilerPlugin("org.typelevel" % "kind-projector" % "0.13.2" cross CrossVersion.full)
        )
      case _ => Seq.empty
    }
  }
)

val versionSchemeSettings = Seq(versionScheme := Some("early-semver"))

val publishSettings = Seq(
  organization := "io.scalaland",
  homepage := Some(url("https://scalaland.io/chimney")),
  organizationHomepage := Some(url("https://scalaland.io")),
  licenses := Seq("Apache-2.0" -> url("https://www.apache.org/licenses/LICENSE-2.0")),
  scmInfo := Some(
    ScmInfo(url("https://github.com/scalalandio/chimney/"), "scm:git:git@github.com:scalalandio/chimney.git")
  ),
  startYear := Some(2017),
  developers := List(
    Developer("krzemin", "Piotr Krzemiński", "", url("https://github.com/krzemin")),
    Developer("MateuszKubuszok", "Mateusz Kubuszok", "", url("https://github.com/MateuszKubuszok"))
  ),
  pomExtra := (
    <issueManagement>
      <system>GitHub issues</system>
      <url>https://github.com/scalalandio/chimney/issues</url>
    </issueManagement>
  ),
  publishTo := sonatypePublishToBundle.value,
  publishMavenStyle := true,
  Test / publishArtifact := false,
  pomIncludeRepository := { _ =>
    false
  },
  // Sonatype ignores isSnapshot setting and only looks at -SNAPSHOT suffix in version:
  //   https://central.sonatype.org/publish/publish-maven/#performing-a-snapshot-deployment
  // meanwhile sbt-git used to set up SNAPSHOT if there were uncommitted changes:
  //   https://github.com/sbt/sbt-git/issues/164
  // (now this suffix is empty by default) so we need to fix it manually.
  git.gitUncommittedChanges := git.gitCurrentTags.value.isEmpty,
  git.uncommittedSignifier := Some("SNAPSHOT")
) ++ (if (isRelease) Seq(scalacOptions -= "-Xfatal-warnings") else Seq.empty)

val mimaSettings = Seq(
  mimaPreviousArtifacts := {
    val previousVersions = moduleName.value match {
      case "chimney-macro-commons"    => Set() // we're not guaranteeing stability of this library just yet
      case "chimney"                  => Set("0.8.0-RC1", "0.8.0")
      case "chimney-cats"             => Set("0.8.0-RC1", "0.8.0")
      case "chimney-java-collections" => Set("0.8.0")
      case "chimney-protobufs"        => Set("0.8.0")
      case _                          => Set()
    }
    previousVersions.map(organization.value %% moduleName.value % _)
  }
)

val noPublishSettings =
  Seq(publish / skip := true, publishArtifact := false)

val ciCommand = (platform: String, scalaSuffix: String) => {
  val isJVM = platform == "JVM"

  val clean = Vector("clean")
  def withCoverage(tasks: String*): Vector[String] =
    "coverage" +: tasks.toVector :+ "coverageAggregate" :+ "coverageOff"

  val projects = (Vector("chimney", "chimneyCats", "chimneyProtobufs") ++ (if (isJVM) Vector("chimneyJavaCollections")
                                                                           else Vector.empty))
    .map(name => s"$name${if (isJVM) "" else platform}$scalaSuffix")
  def tasksOf(name: String): Vector[String] = projects.map(project => s"$project/$name")

  val tasks = if (isJVM) {
    clean ++
      withCoverage((tasksOf("compile") ++ tasksOf("test") ++ tasksOf("coverageReport")).toSeq*) ++
      Vector("benchmarks/compile") ++
      tasksOf("mimaReportBinaryIssues")
  } else {
    clean ++ tasksOf("test")
  }

  tasks.mkString(";")
}

val releaseCommand = (tag: Seq[String]) => if (tag.nonEmpty) "publishSigned;sonatypeBundleRelease" else "publishSigned"

// modules

lazy val root = project
  .in(file("."))
  .enablePlugins(GitVersioning, GitBranchPrompt)
  .settings(settings)
  .settings(publishSettings)
  .settings(noPublishSettings)
  .aggregate(
    (chimneyMacroCommons.projectRefs ++ chimney.projectRefs ++ chimneyCats.projectRefs ++ chimneyJavaCollections.projectRefs ++ chimneyProtobufs.projectRefs)*
  )
  .settings(
    moduleName := "chimney-build",
    name := "chimney-build",
    description := "Build setup for Chimney modules",
    logo :=
      s"""Chimney ${(version).value} build for (${versions.scala212}, ${versions.scala213}, ${versions.scala3}) x (Scala JVM, Scala.js $scalaJSVersion, Scala Native $nativeVersion)
         |
         |This build uses sbt-projectmatrix with sbt-commandmatrix helper:
         | - Scala JVM adds no suffix to a project name seen in build.sbt
         | - Scala.js adds the "JS" suffix to a project name seen in build.sbt
         | - Scala Native adds the "Native" suffix to a project name seen in build.sbt
         | - Scala 2.12 adds the suffix "2_12" to a project name seen in build.sbt
         | - Scala 2.13 adds no suffix to a project name seen in build.sbt
         | - Scala 3 adds the suffix "3" to a project name seen in build.sbt
         |
         |When working with IntelliJ or Scala Metals, edit "val ideScala = ..." and "val idePlatform = ..." within "val versions" in build.sbt to control which Scala version you're currently working with.""".stripMargin,
    usefulTasks := Seq(
      sbtwelcome.UsefulTask("projects", "List all projects generated by the build matrix").noAlias,
      sbtwelcome
        .UsefulTask(
          "test",
          "Compile and test all projects in all Scala versions and platforms (beware! it uses a lot of memory and might OOM!)"
        )
        .noAlias,
      sbtwelcome.UsefulTask("chimney3/console", "Drop into REPL with Chimney DSL imported (3)").noAlias,
      sbtwelcome.UsefulTask("chimney/console", "Drop into REPL with Chimney DSL imported (2.13)").noAlias,
      sbtwelcome
        .UsefulTask(releaseCommand(git.gitCurrentTags.value), "Publish everything to release or snapshot repository")
        .alias("ci-release"),
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

lazy val chimneyMacroCommons = projectMatrix
  .in(file("chimney-macro-commons"))
  .someVariations(versions.scalas, versions.platforms)(only1VersionInIDE*)
  .enablePlugins(GitVersioning, GitBranchPrompt)
  .disablePlugins(WelcomePlugin, ProtocPlugin)
  .settings(
    moduleName := "chimney-macro-commons",
    name := "chimney-macro-commons",
    description := "Utilities for writing cross-platform macro logic"
  )
  .settings(settings*)
  .settings(versionSchemeSettings*)
  .settings(publishSettings*)
  .settings(dependencies*)
  .settings(
    mimaFailOnNoPrevious := false // we're not guaranteeing stability of this library just yet
  )

lazy val chimney = projectMatrix
  .in(file("chimney"))
  .someVariations(versions.scalas, versions.platforms)((non212tests +: only1VersionInIDE)*)
  .enablePlugins(GitVersioning, GitBranchPrompt)
  .disablePlugins(WelcomePlugin, ProtocPlugin)
  .settings(
    moduleName := "chimney",
    name := "chimney",
    description := "Scala library for boilerplate-free data rewriting"
  )
  .settings(settings*)
  .settings(versionSchemeSettings*)
  .settings(publishSettings*)
  .settings(mimaSettings*)
  .settings(dependencies*)
  .settings(
    Compile / console / initialCommands := "import io.scalaland.chimney.*, io.scalaland.chimney.dsl.*",
    Compile / doc / scalacOptions ++= {
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((3, _)) => Seq("-skip-by-regex:io\\.scalaland\\.chimney\\.internal")
        case Some((2, _)) => Seq("-skip-packages", "io.scalaland.chimney.internal")
        case _            => Seq.empty
      }
    },
    // changes to macros should not cause any runtime problems
    mimaBinaryIssueFilters := Seq(ProblemFilters.exclude[Problem]("io.scalaland.chimney.internal.compiletime.*"))
  )
  .dependsOn(chimneyMacroCommons)

lazy val chimneyCats = projectMatrix
  .in(file("chimney-cats"))
  .someVariations(versions.scalas, versions.platforms)(only1VersionInIDE*)
  .enablePlugins(GitVersioning, GitBranchPrompt)
  .disablePlugins(WelcomePlugin, ProtocPlugin)
  .settings(
    moduleName := "chimney-cats",
    name := "chimney-cats",
    description := "Integrations with selected Cats data types and type classes"
  )
  .settings(settings*)
  .settings(versionSchemeSettings*)
  .settings(publishSettings*)
  .settings(mimaSettings*)
  .settings(dependencies*)
  .settings(libraryDependencies += "org.typelevel" %%% "cats-core" % "2.10.0" % "provided")
  .dependsOn(chimney % "test->test;compile->compile")

lazy val chimneyJavaCollections = projectMatrix
  .in(file("chimney-java-collections"))
  .someVariations(versions.scalas, List(VirtualAxis.jvm))(only1VersionInIDE*)
  .enablePlugins(GitVersioning, GitBranchPrompt)
  .disablePlugins(WelcomePlugin)
  .settings(
    moduleName := "chimney-java-collections",
    name := "chimney-java-collections",
    description := "Integrations with selected Java collections"
  )
  .settings(settings*)
  .settings(publishSettings*)
  .settings(mimaSettings*)
  .settings(
    // Scala 2.12 doesn't have scala.jdk.StreamConverters and we use it in test of java.util.stream type class instances
    libraryDependencies += "org.scala-lang.modules" %%% "scala-java8-compat" % "1.0.2" % Test,
    Compile / doc / scalacOptions ++= {
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((3, _)) => Seq("-skip-by-regex:io\\.scalaland\\.chimney\\.javacollections\\.internal")
        case Some((2, _)) => Seq("-skip-packages", "io.scalaland.chimney.javacollections.internal")
        case _            => Seq.empty
      }
    }
  )
  .dependsOn(chimney % "test->test;compile->compile")

lazy val chimneyProtobufs = projectMatrix
  .in(file("chimney-protobufs"))
  .someVariations(versions.scalas, versions.platforms)(
    (only1VersionInIDE :+ MatrixAction
      .ForPlatforms(VirtualAxis.js, VirtualAxis.native)
      .Settings(
        // Scala.js and Scala Native decided to not implement java.time as let external library do it
        // while we want to provide some type class instances for types in java.time
        libraryDependencies += "io.github.cquiroz" %%% "scala-java-time" % "2.5.0"
      ))*
  )
  .enablePlugins(GitVersioning, GitBranchPrompt)
  .disablePlugins(WelcomePlugin)
  .settings(
    moduleName := "chimney-protobufs",
    name := "chimney-protobufs",
    description := "Integrations with selected Protobufs build-in types"
  )
  .settings(settings*)
  .settings(publishSettings*)
  .settings(mimaSettings*)
  .settings(
    scalacOptions := {
      // protobufs Compile contains only generated classes, and scalacOptions from settings:* breaks Scala 3 compilation
      val resetOptions = if (scalacOptions.value.contains("-scalajs")) Seq("-scalajs") else Seq.empty
      val reAddNecessary = CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, 12)) =>
          Seq(
            "-language:higherKinds"
          )
        case _ => Seq.empty
      }
      resetOptions ++ reAddNecessary
    },
    Compile / PB.targets := Seq(scalapb.gen() -> (Compile / sourceManaged).value / "scalapb"),
    Test / PB.protoSources += PB.externalSourcePath.value,
    Test / PB.targets := Seq(scalapb.gen() -> (Test / sourceManaged).value / "scalapb"),
    libraryDependencies += "com.thesamet.scalapb" %%% "scalapb-runtime" % scalapb.compiler.Version.scalapbVersion % "protobuf"
  )
  .dependsOn(chimney % "test->test;compile->compile")

lazy val benchmarks = projectMatrix
  .in(file("benchmarks"))
  .someVariations(List(versions.scala213), List(VirtualAxis.jvm))(only1VersionInIDE*) // only makes sense for JVM
  .settings(
    moduleName := "chimney-benchmarks",
    name := "chimney-benchmarks",
    description := "Chimney benchmarking harness"
  )
  .enablePlugins(JmhPlugin)
  .disablePlugins(WelcomePlugin, ProtocPlugin)
  .settings(settings*)
  .settings(noPublishSettings*)
  .dependsOn(chimney)

//when having memory/GC-related errors during build, uncommenting this may be useful:
Global / concurrentRestrictions := Seq(
  Tags.limit(Tags.Compile, 2) // only 2 compilations at once
)
