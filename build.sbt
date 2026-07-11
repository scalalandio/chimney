import com.jsuereth.sbtpgp.PgpKeys.publishSigned
import com.typesafe.tools.mima.core.{Problem, ProblemFilters}
import commandmatrix.extra.*
import sbtprotoc.ProtocPlugin.ProtobufConfig
import scoverage.ScoverageKeys.coverageScalacPluginVersion

// Used to configure the build so that it would format on compile during development but not on CI.
lazy val isCI = sys.env.get("CI").contains("true")
ThisBuild / scalafmtOnCompile := !isCI

// Used to publish snapshots to Maven Central.
val mavenCentralSnapshots = "Maven Central Snapshots" at "https://central.sonatype.com/repository/maven-snapshots"

// TODO: remove this once we have a release of Scala 2.13.17
Global / resolvers += "scala-integration" at "https://scala-ci.typesafe.com/artifactory/scala-integration/"

// TODO(hearth-migration): REMOVE once versions.hearth is back on a release (see the LOUD WARNING at versions.hearth).
// Global so that both library dependencies AND the Scala 3 hearth-cross-quotes compilerPlugin resolve the SNAPSHOT.
Global / resolvers += mavenCentralSnapshots

// Versions:

val versions = new {
  // Versions we are publishing for.
  val scala213 = "2.13.18"
  val scala3 = "3.8.4"
  // For chimney-sandwich-test-cases-3 ONLY: sbt forbids Scala 2.13 subprojects from depending on Scala 3.8+
  // subprojects (sbt-8728), and 2.13's -Ytasty-reader tops out below TASTy 28.8 - see the module for details.
  val scala3Sandwich = "3.7.3"

  // Which versions should be cross-compiled for publishing
  val scalas = List(scala213, scala3)

  val platforms = List(VirtualAxis.jvm, VirtualAxis.js, VirtualAxis.native)

  // Dependencies.
  // !!! TODO(hearth-migration) LOUD WARNING !!! -----------------------------------------------------------------
  // !!! SNAPSHOT PIN: 0.4.0-16-gd4adc1c-SNAPSHOT is a MOVING TARGET from Maven Central Snapshots (resolver below).
  // !!! It MUST be replaced by a proper hearth RELEASE (and the snapshots resolver removed again)
  // !!! BEFORE merging PR #903. Do NOT release/merge with a -SNAPSHOT hearth dependency.
  // !!! -----------------------------------------------------------------------------------------------------------
  val hearth = "0.4.0-55-g60be58f-SNAPSHOT"
  val cats = "2.13.0"
  // Latest published kindlings (its 0.3.0 depends on hearth 0.4.0, same as us; publishes JVM/JS/Native x 2.13/3).
  // TODO(kindlings-release): snapshot carrying kubuszok/kindlings#163 (NonEmptySeq/NonEmptyLazyList IsCollection
  // providers). Return to a released kindlings before merging PR #903.
  val kindlingsCatsIntegration = "0.3.0-24-gfc36d68-SNAPSHOT"
  val kindProjector = "0.13.4"
  val munit = "1.3.3"
  val scalaJavaCompat = "1.0.2"
  val scalaJavaTime = "2.7.0"
  val scalapbRuntime = scalapb.compiler.Version.scalapbVersion

  // Explicitly handle Scala 2.13 and Scala 3 separately.
  def fold[A](scalaVersion: String)(for2_13: => Seq[A], for3: => Seq[A]): Seq[A] =
    CrossVersion.partialVersion(scalaVersion) match {
      case Some((2, 13)) => for2_13
      case Some((3, _))  => for3
      case _             => Seq.empty // for sbt
    }
}

// Development settings:

val dev = new {

  val props = scala.util
    .Using(new java.io.FileInputStream("dev.properties")) { fis =>
      val props = new java.util.Properties()
      props.load(fis)
      props
    }
    .get

  // Which version should be used in IntelliJ
  val ideScala = props.getProperty("ide.scala") match {
    case "2.13" => versions.scala213
    case "3"    => versions.scala3
  }
  val idePlatform = props.getProperty("ide.platform") match {
    case "jvm"    => VirtualAxis.jvm
    case "js"     => VirtualAxis.js
    case "native" => VirtualAxis.native
  }

  def isIdeScala(scalaVersion: String): Boolean =
    CrossVersion.partialVersion(scalaVersion) == CrossVersion.partialVersion(ideScala)
  def isIdePlatform(platform: VirtualAxis): Boolean = platform == idePlatform
}

// Common settings:

Global / excludeLintKeys += git.useGitDescribe
Global / excludeLintKeys += ideSkipProject
val only1VersionInIDE =
  // For the platform we are working with, show only the project for the Scala version we are working with.
  MatrixAction
    .ForPlatform(dev.idePlatform)
    .Configure(
      _.settings(
        ideSkipProject := !dev.isIdeScala(scalaVersion.value),
        bspEnabled := dev.isIdeScala(scalaVersion.value),
        scalafmtOnCompile := !isCI
      )
    ) +:
    // Do not show in IDE and BSP projects for the platform we are not working with.
    versions.platforms.filterNot(dev.isIdePlatform).map { platform =>
      MatrixAction
        .ForPlatform(platform)
        .Configure(_.settings(ideSkipProject := true, bspEnabled := false, scalafmtOnCompile := false))
    }

val settings = Seq(
  git.useGitDescribe := true,
  git.uncommittedSignifier := None,
  scalacOptions ++= versions.fold(scalaVersion.value)(
    for3 = Seq(
      // format: off
      "-encoding", "UTF-8",
      "-release", "17", // Chimney 2.x baseline: Scala 3 artifacts target JDK 17+ (Hearth itself requires JDK 11+)
      // "-rewrite", // in tests removes case classe used for error message testing
      // "-source", "3.3-migration",
      // format: on
      "-unchecked",
      "-deprecation",
      "-explain",
      "-explain-types",
      "-feature",
      "-no-indent",
      "-Wconf:msg=Unreachable case:s", // suppress fake (?) errors in internal.compiletime
      "-Wconf:msg=Missing symbol position:s", // suppress warning https://github.com/scala/scala3/issues/21672
      "-Wconf:msg=Implicit parameters should be provided with a `using` clause:s", // we're not rewriting this, since we are still cross-compiling with 2.13
      "-Wconf:msg=The syntax `<function> _` is no longer supported:s", // we're not rewriting this, since we are still cross-compiling with 2.13
      "-Wconf:msg=The trailing ` _` for eta-expansion is unnecessary:s", // Scala 3.8 wording of the warning above
      "-Wconf:msg=uninitialized.:s", // we're not rewriting this, since we are still cross-compiling with 2.13
      "-Wnonunit-statement",
      // "-Wunused:imports", // import x.Underlying as X is marked as unused even though it is! probably one of https://github.com/scala/scala3/issues/: #18564, #19252, #19657, #19912
      "-Wunused:privates",
      // "-Wunused:locals",
      "-Wunused:explicits",
      "-Wunused:implicits",
      "-Wunused:params",
      "-Wvalue-discard",
      "-Werror", // -Xfatal-warnings is a deprecated alias since Scala 3.8
      "-Xcheck-macros",
      "-Xkind-projector:underscores",
      "Yimplicit-to-given"
    ),
    for2_13 = Seq(
      // format: off
      "-encoding", "UTF-8",
      "-release", "11", // Chimney 2.x baseline: Scala 2.13 artifacts target JDK 11+ (Hearth built-ins emit JDK 9+ APIs like java.util.Map.entry - documented upstream since hearth#330)
      // format: on
      "-unchecked",
      "-deprecation",
      "-explaintypes",
      "-feature",
      "-language:higherKinds",
      "-Wconf:origin=scala.collection.compat.*:s", // type aliases without which 2.12 fail compilation but 2.13/3 doesn't need them
      "-Wconf:cat=scala3-migration:s", // silence mainly issues with -Xsource:3 and private case class constructors
      "-Wconf:cat=deprecation&origin=io.scalaland.chimney.*:s", // we want to be able to deprecate APIs and test them while they're deprecated
      "-Wconf:msg=The outer reference in this type test cannot be checked at run time:s", // suppress fake(?) errors in internal.compiletime (adding origin breaks this suppression)
      "-Wconf:src=io/scalaland/chimney/cats/package.scala:s", // silence package object inheritance deprecation
      "-Wconf:msg=discarding unmoored doc comment:s", // silence errors when scaladoc cannot comprehend nested vals
      "-Wconf:msg=Could not find any member to link for:s", // since errors when scaladoc cannot link to stdlib types or nested types
      "-Wconf:msg=Variable .+ undefined in comment for:s", // silence errors when there we're showing a buggy Expr in scaladoc comment
      "-Wconf:msg=a type was inferred to be kind-polymorphic `Nothing` to conform to:s", // silence warn that appeared after updating to Scala 2.13.18
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
      "-Xsource-features:eta-expand-always", // silence warn that appears since 2.13.17
      "-Ywarn-dead-code",
      "-Ywarn-numeric-widen",
      "-Ywarn-unused:locals",
      "-Ywarn-unused:imports",
      "-Ywarn-macros:after",
      "-Ytasty-reader"
    )
  ),
  Test / compile / scalacOptions ++= versions.fold(scalaVersion.value)(
    for3 = Seq(
      "-Wconf:msg=unused local definition:s", // silence warn that appears since 3.3.7
      "-Wconf:msg=with as a type operator has been deprecated:s" // silence deprecation of `with` in Scala 3.4+
    ),
    for2_13 = Seq.empty
  ),
  Compile / doc / scalacOptions ++= versions.fold(scalaVersion.value)(
    for3 = Seq("-Ygenerate-inkuire"), // type-based search for Scala 3, this option cannot go into compile
    for2_13 = Seq.empty
  ),
  Compile / console / scalacOptions --= Seq("-Ywarn-unused:imports", "-Xfatal-warnings", "-Werror"),
  Test / compile / scalacOptions --= versions.fold(scalaVersion.value)(
    for3 = Seq.empty,
    for2_13 = Seq.empty
  ),
  coverageExcludedPackages := ".*DefCache.*" // DefCache is kind-a experimental utility
)

val dependencies = Seq(
  libraryDependencies ++= Seq(
    "org.scalameta" %%% "munit" % versions.munit % Test
  ),
  libraryDependencies ++= versions.fold(scalaVersion.value)(
    for3 = Seq.empty,
    for2_13 = Seq(
      "org.scala-lang" % "scala-reflect" % scalaVersion.value % Provided,
      compilerPlugin("org.typelevel" % "kind-projector" % versions.kindProjector cross CrossVersion.full)
    )
  )
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
  publishTo := {
    if (isSnapshot.value) Some(mavenCentralSnapshots)
    else localStaging.value
  },
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
)

val mimaSettings = Seq(
  mimaPreviousArtifacts := {
    val previousVersions = moduleName.value match {
      case "chimney" | "chimney-cats" | "chimney-protobufs" => Set()
      // TODO: restore after 2.0.0 release
      case _ => Set()
    }
    previousVersions.map(organization.value %% moduleName.value % _)
  },
  mimaFailOnNoPrevious := false // true
)

val noPublishSettings =
  Seq(publish / skip := true, publishArtifact := false)

val ciCommand = (platform: String, scalaSuffix: String) => {
  val isJVM = platform == "JVM"
  val isSandwichable = isJVM

  val clean = Vector("clean")
  def withCoverage(tasks: String*): Vector[String] =
    "coverage" +: tasks.toVector :+ "coverageAggregate" :+ "coverageOff"

  val projects = for {
    name <- Vector(
      "chimney",
      "chimneyCats",
      "chimneyProtobufs",
      if (isJVM) "chimneyJavaCollections" else "",
      if (isSandwichable) "chimneySandwichTests" else ""
    )
    if name.nonEmpty
  } yield s"$name${if (isJVM) "" else platform}$scalaSuffix"
  def tasksOf(name: String): Vector[String] = projects.map(project => s"$project/$name")

  val tasks = if (isJVM) {
    clean ++
      withCoverage((tasksOf("compile") ++ tasksOf("test") ++ tasksOf("coverageReport")).toSeq *) ++
      Vector("benchmarks/compile") ++
      tasksOf("mimaReportBinaryIssues")
  } else {
    clean ++ tasksOf("test")
  }

  tasks.mkString(" ; ")
}

val publishLocalForTests = {
  val jvm = for {
    module <- Vector("chimney", "chimneyCats", "chimneyProtobufs")
    moduleVersion <- Vector(module, module + "3")
  } yield moduleVersion + "/publishLocal"
  val js = for {
    module <- Vector("chimney").map(_ + "JS")
    moduleVersion <- Vector(module)
  } yield moduleVersion + "/publishLocal"
  jvm ++ js
}.mkString(" ; ")

val releaseCommand = (tag: Seq[String]) => if (tag.nonEmpty) "publishSigned ; sonaRelease" else "publishSigned"

// modules

lazy val root = project
  .in(file("."))
  .enablePlugins(GitVersioning, GitBranchPrompt)
  .settings(settings)
  .settings(publishSettings)
  .settings(noPublishSettings)
  .aggregate(chimney.projectRefs *)
  .aggregate(chimneyEngineTestExtension.projectRefs *)
  .aggregate(chimneyChimneyExtensionTest.projectRefs *)
  .aggregate(chimneyCats.projectRefs *)
  .aggregate(chimneyJavaCollections.projectRefs *)
  .aggregate(chimneyProtobufs.projectRefs *)
  .aggregate(chimneySandwichTests.projectRefs *)
  .settings(
    moduleName := "chimney-build",
    name := "chimney-build",
    description := "Build setup for Chimney modules",
    logo :=
      s"""Chimney ${(version).value} build for (${versions.scala213}, ${versions.scala3}) x (Scala JVM, Scala.js $scalaJSVersion, Scala Native $nativeVersion)
         |
         |This build uses sbt-projectmatrix with sbt-commandmatrix helper:
         | - Scala JVM adds no suffix to a project name seen in build.sbt
         | - Scala.js adds the "JS" suffix to a project name seen in build.sbt
         | - Scala Native adds the "Native" suffix to a project name seen in build.sbt
         | - Scala 2.13 adds no suffix to a project name seen in build.sbt
         | - Scala 3 adds the suffix "3" to a project name seen in build.sbt
         |
         |When working with IntelliJ or Scala Metals, edit "val ideScala = ..." and "val idePlatform = ..." within "val versions" in build.sbt to control which Scala version you're currently working with.
         |
         |If you need to test library locally in a different project, use publish-local-for-tests or manually publishLocal:
         | - chimney
         | - cats/protobufs integration (optional)
         |for the right Scala version and platform (see projects task).
         |""".stripMargin,
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
      sbtwelcome.UsefulTask(ciCommand("JS", "3"), "CI pipeline for Scala 3 on Scala JS").alias("ci-js-3"),
      sbtwelcome.UsefulTask(ciCommand("JS", ""), "CI pipeline for Scala 2.13 on Scala JS").alias("ci-js-2_13"),
      sbtwelcome.UsefulTask(ciCommand("Native", "3"), "CI pipeline for Scala 3 on Scala Native").alias("ci-native-3"),
      sbtwelcome
        .UsefulTask(ciCommand("Native", ""), "CI pipeline for Scala 2.13 on Scala Native")
        .alias("ci-native-2_13"),
      sbtwelcome
        .UsefulTask(
          publishLocalForTests,
          "Publishes all Scala 2.13 and Scala 3 JVM artifacts to test snippets in documentation"
        )
        .alias("publish-local-for-tests")
    )
  )

lazy val chimney = projectMatrix
  .in(file("chimney"))
  .someVariations(versions.scalas, versions.platforms)(only1VersionInIDE *)
  .enablePlugins(GitVersioning, GitBranchPrompt)
  .disablePlugins(WelcomePlugin, ProtocPlugin)
  .settings(
    moduleName := "chimney",
    name := "chimney",
    description := "Scala library for boilerplate-free data rewriting"
  )
  .settings(settings *)
  .settings(versionSchemeSettings *)
  .settings(publishSettings *)
  .settings(mimaSettings *)
  .settings(dependencies *)
  .settings(
    Compile / console / initialCommands := "import io.scalaland.chimney.*, io.scalaland.chimney.dsl.*",
    Compile / doc / scalacOptions ++= versions.fold(scalaVersion.value)(
      for3 = Seq("-skip-by-regex:io\\.scalaland\\.chimney\\.internal"),
      for2_13 = Seq("-skip-packages", "io.scalaland.chimney.internal")
    ),
    libraryDependencies += "com.kubuszok" %%% "hearth" % versions.hearth,
    // ChimneySpec is based on hearth-munit's MacroSuite (group/==>/compileErrors-check utilities).
    libraryDependencies += "com.kubuszok" %%% "hearth-munit" % versions.hearth % Test,
    // Cross-quotes: on Scala 2 they are macros (part of hearth), on Scala 3 they are a compiler plugin.
    libraryDependencies ++= versions.fold(scalaVersion.value)(
      for2_13 = Seq.empty,
      for3 = Seq(compilerPlugin("com.kubuszok" %% "hearth-cross-quotes" % versions.hearth))
    ),
    // Changes to macros should not cause any runtime problems
    mimaBinaryIssueFilters := Seq(ProblemFilters.exclude[Problem]("io.scalaland.chimney.internal.compiletime.*"))
  )
  // Test-only Hearth StandardMacroExtension (ServiceLoader-registered) proving that third-party extensions are
  // consulted by the engine's built-in fallbacks. Must be a SEPARATE module: extension classes are loaded reflectively
  // at macro-expansion time, so they must be compiled before the test sources that trigger the expansion.
  // NOTE: this puts a test-scoped dependency on an unpublished artifact into the published pom - harmless for
  // consumers (test scope is not transitive), but TODO(hearth-extensions): consider pomPostProcess filtering.
  .dependsOn(chimneyEngineTestExtension % Test)

// Not published - see the comment at the `.dependsOn` in `chimney` above.
lazy val chimneyEngineTestExtension = projectMatrix
  .in(file("chimney-engine-test-extension"))
  .someVariations(versions.scalas, versions.platforms)(only1VersionInIDE *)
  .enablePlugins(GitVersioning, GitBranchPrompt)
  .disablePlugins(WelcomePlugin, ProtocPlugin)
  .settings(settings *)
  .settings(publishSettings *)
  .settings(noPublishSettings *)
  .settings(dependencies *)
  .settings(
    moduleName := "chimney-engine-test-extension",
    name := "chimney-engine-test-extension",
    description := "Test-only Hearth StandardMacroExtension used by chimney's engine test suites",
    mimaFailOnNoPrevious := false, // this module is not published
    libraryDependencies += "com.kubuszok" %%% "hearth" % versions.hearth,
    // Cross-quotes: on Scala 2 they are macros (part of hearth), on Scala 3 they are a compiler plugin.
    libraryDependencies ++= versions.fold(scalaVersion.value)(
      for2_13 = Seq.empty,
      for3 = Seq(compilerPlugin("com.kubuszok" %% "hearth-cross-quotes" % versions.hearth))
    )
  )

// Test-only proof of Chimney's OWN engine-aware macro-extension SPI (io.scalaland.chimney.integrations.ChimneyMacroExtension).
// Unlike chimneyEngineTestExtension (which only implements Hearth's StdExtensions and so needs no chimney dep), this
// module IMPLEMENTS a Chimney SPI, so it needs chimney on the Compile classpath. It therefore CANNOT be depended upon by
// chimney (that would cycle); instead it depends on chimney and hosts its OWN specs (the chimney-protobufs pattern),
// which prove the ServiceLoader-registered handler is consulted from a SEPARATELY-COMPILED artifact.
lazy val chimneyChimneyExtensionTest = projectMatrix
  .in(file("chimney-chimney-extension-test"))
  .someVariations(versions.scalas, versions.platforms)(only1VersionInIDE *)
  .enablePlugins(GitVersioning, GitBranchPrompt)
  .disablePlugins(WelcomePlugin, ProtocPlugin)
  .settings(settings *)
  .settings(publishSettings *)
  .settings(noPublishSettings *)
  .settings(dependencies *)
  .settings(
    moduleName := "chimney-chimney-extension-test",
    name := "chimney-chimney-extension-test",
    description := "Test-only Chimney ChimneyMacroExtension used to prove the engine-aware macro-extension SPI",
    mimaFailOnNoPrevious := false, // this module is not published
    libraryDependencies += "com.kubuszok" %%% "hearth" % versions.hearth,
    // Cross-quotes: on Scala 2 they are macros (part of hearth), on Scala 3 they are a compiler plugin.
    libraryDependencies ++= versions.fold(scalaVersion.value)(
      for2_13 = Seq.empty,
      for3 = Seq(compilerPlugin("com.kubuszok" %% "hearth-cross-quotes" % versions.hearth))
    )
  )
  .dependsOn(chimney % s"$Test->$Test;$Compile->$Compile")

lazy val chimneyCats = projectMatrix
  .in(file("chimney-cats"))
  .someVariations(versions.scalas, versions.platforms)(only1VersionInIDE *)
  .enablePlugins(GitVersioning, GitBranchPrompt)
  .disablePlugins(WelcomePlugin, ProtocPlugin)
  .settings(
    moduleName := "chimney-cats",
    name := "chimney-cats",
    description := "Integrations with selected Cats data types and type classes"
  )
  .settings(settings *)
  .settings(versionSchemeSettings *)
  .settings(publishSettings *)
  .settings(mimaSettings *)
  .settings(dependencies *)
  .settings(
    Compile / console / initialCommands := "import io.scalaland.chimney.*, io.scalaland.chimney.dsl.*, io.scalaland.chimney.cats.*",
    libraryDependencies += "org.typelevel" %%% "cats-core" % versions.cats,
    libraryDependencies += "org.typelevel" %%% "cats-laws" % versions.cats % Test,
    // Since 2.0.0 chimney-cats also ships a Chimney `ChimneyMacroExtension` (ServiceLoader-registered, see
    // src/main/resources/META-INF/services): engine-aware SpecialCaseHandlers restoring the total NonEmpty<->NonEmpty
    // (Traverse), NonEmptyMap/NonEmptySet and FunctionK conversions that used to live as `CatsDataImplicits`. The
    // handlers use Hearth's API + cross-quotes directly (hearth itself already comes transitively through chimney).
    libraryDependencies += "com.kubuszok" %%% "hearth" % versions.hearth,
    // Cross-quotes: on Scala 2 they are macros (part of hearth), on Scala 3 they are a compiler plugin.
    libraryDependencies ++= versions.fold(scalaVersion.value)(
      for2_13 = Seq.empty,
      for3 = Seq(compilerPlugin("com.kubuszok" %% "hearth-cross-quotes" % versions.hearth))
    ),
    // Hearth StandardMacroExtension with IsCollection/IsMap providers for cats.data types (NonEmptyList, Chain, ...).
    // Test-scoped: it is consulted at MACRO-EXPANSION time of the TEST sources (ServiceLoader on the compile
    // classpath of the code being derived) - the specs prove cats collections derive WITHOUT chimney-cats implicits.
    // NOTE: kindlings' Scala 3 artifacts are built with Scala 3.8.x (TASTy 28.8) - loading them requires chimney to
    // build with Scala 3.8+ (older compilers throw "Forward incompatible TASTy file" from hearth's extension loading).
    // Since hearth#325 (0.4.1) an unloadable extension jar is SKIPPED gracefully instead of poisoning every derivation
    // in the module - but the specs here obviously still need the extension to actually load.
    libraryDependencies += "com.kubuszok" %%% "kindlings-cats-integration" % versions.kindlingsCatsIntegration % Test
  )
  .dependsOn(chimney % s"$Test->$Test;$Compile->$Compile")

// Since 2.0.0 this module is NOT published and contains NO implicits: Hearth's built-in std-extension providers
// (consulted by the engine's extension-fallback layer) support java.util collections, java.util.Optional and Java
// boxed primitives out of the box, without any import. The module remains as a test-only proof of that coverage
// (every type previously served by JavaCollectionsImplicits/JavaPrimitivesImplicits is still asserted here).
lazy val chimneyJavaCollections = projectMatrix
  .in(file("chimney-java-collections"))
  .someVariations(versions.scalas, List(VirtualAxis.jvm))(only1VersionInIDE *)
  .enablePlugins(GitVersioning, GitBranchPrompt)
  .disablePlugins(WelcomePlugin)
  .settings(
    moduleName := "chimney-java-collections",
    name := "chimney-java-collections",
    description := "Tests proving that java.util types are supported by Chimney out of the box (via Hearth std extensions)"
  )
  .settings(settings *)
  .settings(publishSettings *)
  .settings(noPublishSettings *)
  .settings(
    mimaFailOnNoPrevious := false // this module is not published
  )
  .dependsOn(chimney % s"$Test->$Test;$Compile->$Compile")

lazy val chimneyProtobufs = projectMatrix
  .in(file("chimney-protobufs"))
  .someVariations(versions.scalas, versions.platforms)(
    (only1VersionInIDE :+ MatrixAction
      .ForPlatforms(VirtualAxis.js, VirtualAxis.native)
      .Settings(
        // Scala.js and Scala Native decided to not implement java.time and let an external library do it,
        // meanwhile we want to provide some type class instances for types in java.time.
        libraryDependencies += "io.github.cquiroz" %%% "scala-java-time" % versions.scalaJavaTime
      )) *
  )
  .enablePlugins(GitVersioning, GitBranchPrompt)
  .disablePlugins(WelcomePlugin)
  .settings(
    moduleName := "chimney-protobufs",
    name := "chimney-protobufs",
    description := "Integrations with selected Protobufs build-in types"
  )
  .settings(settings *)
  .settings(versionSchemeSettings *)
  .settings(publishSettings *)
  .settings(mimaSettings *)
  .settings(
    Compile / console / initialCommands := "import io.scalaland.chimney.*, io.scalaland.chimney.dsl.*, io.scalaland.chimney.protobufs.*",
    scalacOptions := {
      // protobufs Compile mixes scalapb-generated classes with hand-written sources, and the strict scalacOptions
      // from settings:* break the generated code - keep the options minimal (compiler-plugin flags like -Xplugin/
      // -scalajs are injected by sbt AFTER this setting, so cross-quotes/Scala.js keep working); the hand-written
      // sources are therefore written in the -Xsource:3-free common syntax subset (see ProtobufsMacroExtension).
      if (scalacOptions.value.contains("-scalajs")) Seq("-scalajs") else Seq.empty
    },
    // Since 2.0.0 chimney-protobufs also ships a Hearth StandardMacroExtension (ServiceLoader-registered, see
    // src/main/resources/META-INF/services): std-extension providers for ByteString/wrappers.*Value/Timestamp
    // replaced the corresponding implicits, so those conversions work WITHOUT any import once this jar is on the
    // classpath. The providers use Hearth's API + cross-quotes directly, hence the explicit dependencies below
    // (hearth itself already comes transitively through the chimney module).
    libraryDependencies += "com.kubuszok" %%% "hearth" % versions.hearth,
    // Cross-quotes: on Scala 2 they are macros (part of hearth), on Scala 3 they are a compiler plugin.
    libraryDependencies ++= versions.fold(scalaVersion.value)(
      for2_13 = Seq.empty,
      for3 = Seq(compilerPlugin("com.kubuszok" %% "hearth-cross-quotes" % versions.hearth))
    ),
    Compile / PB.targets := Seq(scalapb.gen() -> (Compile / sourceManaged).value / "scalapb"),
    Test / PB.protoSources += PB.externalSourcePath.value,
    Test / PB.targets := Seq(scalapb.gen() -> (Test / sourceManaged).value / "scalapb"),
    libraryDependencies += "com.thesamet.scalapb" %% "scalapb-runtime" % versions.scalapbRuntime % ProtobufConfig
  )
  .dependsOn(chimney % s"$Test->$Test;$Compile->$Compile")

lazy val chimneySandwichTestCases213 = projectMatrix
  .in(file("chimney-sandwich-test-cases-213"))
  .someVariations(List(versions.scala213), List(VirtualAxis.jvm))()
  .settings(settings *)
  .settings(publishSettings *)
  .settings(noPublishSettings *)
  .settings(
    moduleName := "chimney-sandwich-test-cases-213",
    name := "chimney-sandwich-test-cases-213",
    description := "Tests cases compiled with Scala 2.13 to test macros in 2.13x3 cross-compilation",
    mimaFailOnNoPrevious := false // this module is not published
  )

lazy val chimneySandwichTestCases3 = projectMatrix
  .in(file("chimney-sandwich-test-cases-3"))
  .someVariations(List(versions.scala3), List(VirtualAxis.jvm))()
  .settings(settings *)
  .settings(publishSettings *)
  .settings(noPublishSettings *)
  .settings(
    moduleName := "chimney-sandwich-test-cases-3",
    name := "chimney-sandwich-test-cases-3",
    description := "Tests cases compiled with Scala 3 to test macros in 2.13x3 cross-compilation",
    mimaFailOnNoPrevious := false, // this module is not published
    // PINNED to the newest Scala 3 line a Scala 2.13 subproject can still consume: since Scala 3.8 the
    // scala-library coordinate was unified and sbt REFUSES 2.13-depends-on-3.8+ sandwiches outright
    // ("[sbt-8728] Smorrebrod - the end of Scala 2.13-3.x sandwich", resolution asks for the nonexistent
    // org.scala-lang:scala-compiler:3.8.x) - and Scala 2.13's -Ytasty-reader cannot read TASTy 28.8 anyway.
    // This mirrors what real 2.13x3 sandwich users can do, so the fixture stays on 3.7.
    scalaVersion := versions.scala3Sandwich,
    // Unpublished fixture: don't force the JDK 17 bytecode floor - the Scala 2.13 CI job (temurin:11) compiles
    // this module for the 2.13x3 sandwich tests, and -release 17 cannot be honored by a compiler running on JDK 11.
    scalacOptions --= Seq("-release", "17")
  )

lazy val chimneySandwichTests = projectMatrix
  .in(file("chimney-sandwich-tests"))
  .someVariations(List(versions.scala213, versions.scala3), List(VirtualAxis.jvm))(only1VersionInIDE *)
  .settings(settings *)
  .settings(publishSettings *)
  .settings(noPublishSettings *)
  .settings(
    moduleName := "chimney-sandwich-tests",
    name := "chimney-sandwich-tests",
    description := "Tests macros in 2.13x3 cross-compilation",
    mimaFailOnNoPrevious := false // this module is not published
  )
  .dependsOn(chimney % s"$Test->$Test;$Compile->$Compile")
  .dependsOn(chimneySandwichTestCases213 % s"$Test->$Test;$Compile->$Compile")
  .dependsOn(chimneySandwichTestCases3 % s"$Test->$Test;$Compile->$Compile")

lazy val benchmarks = projectMatrix
  .in(file("benchmarks"))
  .someVariations(List(versions.scala213), List(VirtualAxis.jvm))(only1VersionInIDE *) // only makes sense for JVM
  .settings(
    moduleName := "chimney-benchmarks",
    name := "chimney-benchmarks",
    description := "Chimney benchmarking harness"
  )
  .enablePlugins(JmhPlugin)
  .disablePlugins(WelcomePlugin, ProtocPlugin)
  .settings(settings *)
  .settings(noPublishSettings *)
  .dependsOn(chimney)

//when having memory/GC-related errors during build, uncommenting this may be useful:
Global / concurrentRestrictions := Seq(
  Tags.limit(Tags.Compile, 2) // only 2 compilations at once
)
