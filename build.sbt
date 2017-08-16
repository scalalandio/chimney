name := "chimney"

val versions = new {
  val shapelessVersion = "2.3.2"
  val scalatestVersion = "3.0.3"
  val scalafmt = "1.1.0"
  val scalaVersion = "2.12.3"
}

val settings = Seq(
  version := "0.1.5",
  scalaVersion := versions.scalaVersion,
  crossScalaVersions := Seq("2.11.11", "2.12.3"),
  scalacOptions ++= Seq(
    "-target:jvm-1.8",
    "-encoding",
    "UTF-8",
    "-unchecked",
    "-deprecation",
    "-explaintypes",
    "-feature",
    "-language:existentials",
    "-language:higherKinds",
    "-language:implicitConversions",
    "-Yno-adapted-args",
    "-Ywarn-dead-code",
    "-Ywarn-inaccessible",
    "-Ywarn-infer-any",
    "-Ywarn-nullary-override",
    "-Ywarn-nullary-unit",
    "-Ywarn-numeric-widen",
    "-Xfatal-warnings",
    "-Xfuture",
    "-Xlint:adapted-args",
    "-Xlint:by-name-right-associative",
    "-Xlint:delayedinit-select",
    "-Xlint:doc-detached",
    "-Xlint:inaccessible",
    "-Xlint:infer-any",
    "-Xlint:missing-interpolator",
    "-Xlint:nullary-override",
    "-Xlint:nullary-unit",
    "-Xlint:option-implicit",
    "-Xlint:package-object-classes",
    "-Xlint:poly-implicit-overload",
    "-Xlint:private-shadow",
    "-Xlint:stars-align",
    "-Xlint:type-parameter-shadow",
    "-Xlint:unsound-match",
    "-Xexperimental"
  ),
  scalacOptions in (Compile, console) --= Seq("-Ywarn-unused:imports", "-Xfatal-warnings")
)

val dependencies = Seq(
  libraryDependencies ++= Seq(
    "org.scala-lang" % "scala-reflect" % versions.scalaVersion,
    "com.chuusai" %%% "shapeless" % versions.shapelessVersion,
    "org.scalatest" %%% "scalatest" % versions.scalatestVersion % "test"
  )
)

lazy val root = project
  .in(file("."))
  .settings(settings: _*)
  .settings(publishSettings: _*)
  .settings(noPublishSettings: _*)
  .settings(commands += Command.args("scalafmt", "Run scalafmt cli.") {
    case (state, args) =>
      val Right(scalafmt) =
        org.scalafmt.bootstrap.ScalafmtBootstrap.fromVersion(versions.scalafmt)
      scalafmt.main("--non-interactive" +: args.toArray)
      state
  })
  .aggregate(chimneyJVM, chimneyJS, protosJVM, protosJS)
  .dependsOn(chimneyJVM, chimneyJS)

lazy val chimney = crossProject
  .crossType(CrossType.Pure)
  .settings(
    moduleName := "chimney",
    name := "chimney",
    description := "Scala library for boilerplate free data rewriting"
  )
  .settings(settings: _*)
  .settings(publishSettings: _*)
  .settings(dependencies: _*)
  .dependsOn(protos % "test->compile")

lazy val chimneyJVM = chimney.jvm
lazy val chimneyJS = chimney.js

lazy val protos = crossProject
  .crossType(CrossType.Pure)
  .settings(
    name := "chimney-protos",
    libraryDependencies += "com.trueaccord.scalapb" %%% "scalapb-runtime" % com.trueaccord.scalapb.compiler.Version.scalapbVersion,
    PB.targets in Compile := Seq(scalapb.gen() -> (sourceManaged in Compile).value),
    PB.protoSources in Compile := Seq(file("protos/src/main/protobuf")),
    coverageExcludedPackages := "<empty>;(.*)"
  )
  .settings(settings: _*)
  .settings(noPublishSettings: _*)

lazy val protosJVM = protos.jvm
lazy val protosJS = protos.js

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
  Seq(publish := (), publishLocal := (), publishArtifact := false)
