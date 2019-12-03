import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

val versions = new {
  val scalaVersion = "2.13.1"
}

val settings = Seq(
  scalaVersion := versions.scalaVersion,
  crossScalaVersions := Seq("2.11.12", "2.12.10", "2.13.1"),
  scalacOptions ++= Seq(
    "-target:jvm-1.8",
    "-encoding", "UTF-8",
    "-unchecked",
    "-deprecation",
    "-explaintypes",
    "-feature",
    "-Ywarn-dead-code",
    "-Ywarn-numeric-widen",
//    "-Xfatal-warnings",
    "-Xlint:adapted-args",
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
    "-Xlint:type-parameter-shadow"
  ) ++ (
    if (scalaVersion.value >= "2.13")
      Nil
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
        "-Xlint:unsound-match"
      )
    ),
  scalacOptions in (Compile, console) --= Seq("-Ywarn-unused:imports", "-Xfatal-warnings")
)

val dependencies = Seq(
  libraryDependencies ++= Seq(
    "org.scala-lang" % "scala-reflect" % scalaVersion.value % "provided",
    "org.scala-lang" % "scala-compiler" % scalaVersion.value % "provided",
    "com.lihaoyi" %%% "utest" % (if (scalaVersion.value >= "2.12") "0.7.1" else "0.6.8") % "test"
  )
)

lazy val root = project
  .in(file("."))
  .settings(settings: _*)
  .settings(publishSettings: _*)
  .settings(noPublishSettings: _*)
  .aggregate(chimneyJVM, chimneyJS, `chimney-validatedJVM`, `chimney-validatedJs`)
  .dependsOn(chimneyJVM, chimneyJS, `chimney-validatedJVM`, `chimney-validatedJs`)

lazy val chimney = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .dependsOn(protos % "test->test")
  .settings(
    moduleName := "chimney",
    name := "chimney",
    version := "0.3.5",
    description := "Scala library for boilerplate free data rewriting",
    testFrameworks += new TestFramework("utest.runner.Framework")
  )
  .settings(settings: _*)
  .settings(publishSettings: _*)
  .settings(dependencies: _*)
  .nativeSettings(nativeLinkStubs := true)

lazy val chimneyJVM = chimney.jvm
lazy val chimneyJS = chimney.js
lazy val chimneyNative = chimney.native

val commonCats = libraryDependencies += "org.typelevel" %%% "cats-core" % "2.0.0"
val nativeCats = libraryDependencies += "com.github.lolgab" %%% "cats-core" % "2.0.0-M4"

lazy val `chimney-validated` = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .dependsOn(chimney)
  .settings(
    moduleName := "chimney-validated",
    name := "chimney-validated",
    version := "0.0.1",
    description := "Extension module for using error validating transformers",
    testFrameworks += new TestFramework("utest.runner.Framework")
  )
  .settings(settings: _*)
  .settings(publishSettings: _*)
  .settings(dependencies: _*)
  .nativeSettings(nativeCats)
  .jvmSettings(commonCats)
  .jsSettings(commonCats)

lazy val `chimney-validatedJVM` = `chimney-validated`.jvm
lazy val `chimney-validatedJs` = `chimney-validated`.js
lazy val `chimney-validatedNative` = `chimney-validated`.native

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

lazy val readme = scalatex
  .ScalatexReadme(
    projectId = "readme",
    wd = file(""),
    url = "https://github.com/scalalandio/chimney/tree/master",
    source = "Readme"
  )
  .settings(noPublishSettings : _*)
  .settings(
    scalaVersion := "2.12.10",
    siteSourceDirectory := target.value / "scalatex",
    git.remoteRepo := "git@github.com:scalalandio/chimney.git",
    includeFilter in (makeSite in Jekyll) := new FileFilter {
      def accept(p: File) = true
    }
  )
  .enablePlugins(GhpagesPlugin)
