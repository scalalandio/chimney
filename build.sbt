import commandmatrix.extra._

// versions

ThisProject / versionScheme := Some("early-semver")

val versions = new {
  val chimney = "0.6.2"

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
  version := versions.chimney,
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
    "-Xfatal-warnings",
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

// modules

lazy val root = project
  .in(file("."))
  .enablePlugins(SphinxPlugin, GhpagesPlugin)
  .settings(settings: _*)
  .settings(publishSettings: _*)
  .settings(noPublishSettings: _*)
  .aggregate(chimney.projectRefs ++ chimneyCats.projectRefs: _*)
  .settings(
    Sphinx / version := version.value,
    Sphinx / sourceDirectory := file("docs") / "source",
    git.remoteRepo := "git@github.com:scalalandio/chimney.git"
  )

lazy val chimney = projectMatrix
  .in(file("chimney"))
  .someVariations(
    versions.scalas,
    versions.platforms
  )(only1VersionInIDE: _*)
  .settings(
    moduleName := "chimney",
    name := "chimney",
    description := "Scala library for boilerplate free data rewriting"
  )
  .settings(settings: _*)
  .settings(publishSettings: _*)
  .settings(dependencies: _*)
  .dependsOn(protos % "test->test")

lazy val chimneyCats = projectMatrix
  .in(file("chimneyCats"))
  .someVariations(
    versions.scalas,
    versions.platforms
  )(only1VersionInIDE: _*)
  .dependsOn(chimney % "test->test;compile->compile")
  .settings(
    moduleName := "chimney-cats",
    name := "chimney-cats",
    description := "Chimney module for validated transformers support"
  )
  .settings(settings: _*)
  .settings(publishSettings: _*)
  .settings(dependencies: _*)
  .settings(libraryDependencies += "org.typelevel" %%% "cats-core" % "2.8.0" % "provided")

lazy val protos = projectMatrix
  .in(file("protos"))
  .someVariations(
    versions.scalas,
    versions.platforms
  )(only1VersionInIDE: _*)
  .settings(
    moduleName := "chimney-protos",
    name := "chimney-protos",
    description := "Protobufs used for conversion testing"
  )
  .settings(settings: _*)
  .settings(noPublishSettings: _*)

lazy val benchmarks = projectMatrix
  .in(file("benchmarks"))
  .someVariations(
    versions.scalas,
    List(VirtualAxis.jvm) // only makes sense for JVM
  )(only1VersionInIDE: _*)
  .settings(
    moduleName := "chimney-benchmarks",
    name := "chimney-benchmarks",
    description := "Chimney benchmarking harness"
  )
  .enablePlugins(JmhPlugin)
  .settings(settings: _*)
  .settings(noPublishSettings: _*)
  .dependsOn(chimney % "test->test;compile->compile")

// aliases

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
    s"chimney$scalaSuffix/coverageReport",
    "coverageOff",
    s"chimneyCats$scalaSuffix/test",
    s"chimneyJS$scalaSuffix/test",
    s"chimneyCatsJS$scalaSuffix/test",
    s"chimneyNative$scalaSuffix/test",
    s"chimneyCatsNative$scalaSuffix/test",
  ).map(";" + _).mkString

addCommandAlias("ci-2_12", ciCommand("2_12"))
addCommandAlias("ci-2_13", ciCommand(""))
