import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

val versions = new {
  val scala211 = "2.11.12"
  val scala212 = "2.12.12"
  val scala213 = "2.13.3"
}

val settings = Seq(
  version := "0.5.3",
  scalaVersion := versions.scala213,
  crossScalaVersions :=
    (if (scalaJSVersion.startsWith("1.")) Nil else Seq(versions.scala211)) ++
      Seq(versions.scala212, versions.scala213),
  scalacOptions ++= Seq(
    "-target:jvm-1.8",
    "-encoding", "UTF-8",
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
    "-language:higherKinds"
  ),
  scalacOptions ++= (
    if (scalaVersion.value >= "2.13")
      Seq("-Wunused:patvars")
    else
      Seq(
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
  scalacOptions ++= (
    if (scalaVersion.value >= "2.12")
      Seq(
        "-Ywarn-unused:locals",
        "-Ywarn-macros:after",
        "-Xfatal-warnings"
      )
    else
      Nil
    ),
  scalacOptions in (Compile, console) --= Seq("-Ywarn-unused:imports", "-Xfatal-warnings")
)

val dependencies = Seq(
  libraryDependencies ++= Seq(
    "org.scala-lang.modules" %%% "scala-collection-compat" % "2.2.0",
    "org.scala-lang" % "scala-reflect" % scalaVersion.value % "provided",
    "org.scala-lang" % "scala-compiler" % scalaVersion.value % "provided",
    "com.lihaoyi" %%% "utest" % (if (scalaVersion.value >= "2.12") "0.7.5" else "0.6.8") % "test"
  )
)

lazy val root = project
  .in(file("."))
  .settings(settings: _*)
  .settings(publishSettings: _*)
  .settings(noPublishSettings: _*)
  .aggregate(chimneyJVM, chimneyJS, chimneyCatsJVM, chimneyCatsJS)
  .dependsOn(chimneyJVM, chimneyJS, chimneyCatsJVM, chimneyCatsJS)
  .enablePlugins(SphinxPlugin, GhpagesPlugin)
  .settings(
    Sphinx / version := version.value,
    Sphinx / sourceDirectory := file("docs") / "source",
    git.remoteRepo := "git@github.com:scalalandio/chimney.git"
  )

lazy val chimney = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .dependsOn(protos % "test->test")
  .settings(
    moduleName := "chimney",
    name := "chimney",
    description := "Scala library for boilerplate free data rewriting",
    testFrameworks += new TestFramework("utest.runner.Framework"),
    addCompilerPlugin("org.typelevel" % "kind-projector" % "0.11.0" cross CrossVersion.full)
  )
  .settings(settings: _*)
  .settings(publishSettings: _*)
  .settings(dependencies: _*)
  .nativeSettings(nativeLinkStubs := true)

lazy val chimneyJVM = chimney.jvm
lazy val chimneyJS = chimney.js
lazy val chimneyNative = chimney.native

lazy val chimneyCats = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Pure)
  .dependsOn(chimney % "test->test;compile->compile")
  .settings(
    moduleName := "chimney-cats",
    name := "chimney-cats",
    description := "Chimney module for validated transformers support",
    testFrameworks += new TestFramework("utest.runner.Framework"),
    addCompilerPlugin("org.typelevel" % "kind-projector" % "0.11.0" cross CrossVersion.full)
  )
  .settings(settings: _*)
  .settings(publishSettings: _*)
  .settings(dependencies: _*)
  .settings(libraryDependencies += "org.typelevel" %%% "cats-core" % (if (scalaBinaryVersion.value == "2.11") "2.0.0" else "2.2.0") % "provided")

lazy val chimneyCatsJVM = chimneyCats.jvm
lazy val chimneyCatsJS = chimneyCats.js

lazy val protos = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .settings(
    moduleName := "chimney-protos",
    name := "chimney-protos"
  )
  .settings(settings: _*)
  .settings(noPublishSettings: _*)

lazy val protosJVM = protos.jvm
lazy val protosJS = protos.js
lazy val protosNative = protos.native


lazy val publishSettings = Seq(
  organization := "io.scalaland",
  homepage := Some(url("https://scalaland.io")),
  licenses := Seq("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
  scmInfo := Some(
    ScmInfo(url("https://github.com/scalalandio/chimney"), "scm:git:git@github.com:scalalandio/chimney.git")
  ),
  publishTo := {
    val nexus = "https://oss.sonatype.org/"
    if (isSnapshot.value)
      Some("snapshots" at nexus + "content/repositories/snapshots")
    else
      Some("releases" at nexus + "service/local/staging/deploy/maven2")
  },
  publishMavenStyle := true,
  publishArtifact in Test := false,
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

lazy val noPublishSettings =
  Seq(skip in publish := true, publishArtifact := false)
