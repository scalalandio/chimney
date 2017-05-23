name := "chimney"

version := "1.0"
organization := "io.scalaland"
name := "chimney"

scalaVersion := "2.12.2"
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
  "-Xlint",
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
)
scalacOptions in (Compile, console) --= Seq(
  "-Ywarn-unused:imports",
  "-Xfatal-warnings"
)

libraryDependencies += "com.chuusai" %% "shapeless" % "2.3.2"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.3" % "test"
