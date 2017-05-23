val settings = Seq(
  organization := "io.scalaland",
  version := "0.1.0",

  scalaVersion := "2.12.2",
  scalacOptions ++= Seq(
    "-target:jvm-1.8",
    "-encoding", "UTF-8",
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
    "-Xlint:constant",
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
    "-Xlint:unsound-match"
  ),
  scalacOptions in (Compile, console) --= Seq(
    "-Ywarn-unused:imports",
    "-Xfatal-warnings"
  )
)

val versions = new {
  val shapelessVersion = "2.3.2"
  val scalatestVersion = "3.0.3"
}

val dependencies = Seq(
  libraryDependencies += "com.chuusai"   %%% "shapeless" % versions.shapelessVersion,
  libraryDependencies += "org.scalatest" %%% "scalatest" % versions.scalatestVersion % "test"
)

lazy val root = project.in(file("."))
  .settings(settings: _*)
  .aggregate(chimneyJVM, chimneyJS)

lazy val chimney = crossProject.crossType(CrossType.Pure)
  .settings(
    moduleName  := "chimney",
    name        := "chimney",
    description := "Scala library for boilerplate free data rewriting"
  )
  .settings(settings: _*)
  .settings(dependencies: _*)

lazy val chimneyJVM = chimney.jvm

lazy val chimneyJS = chimney.js