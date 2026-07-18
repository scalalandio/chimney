// git
addSbtPlugin("com.github.sbt" % "sbt-git" % "2.1.0")
// linters
addSbtPlugin("org.scalameta" % "sbt-scalafmt" % "2.6.2")
addSbtPlugin("org.scoverage" % "sbt-scoverage" % "2.4.4")
// cross-compile
// sbt-projectmatrix was in-sourced into sbt 2 core (the standalone plugin has no sbt 2 build), so the
// `projectMatrix`/`VirtualAxis`/`MatrixAction` API now comes from sbt itself - no addSbtPlugin needed.
addSbtPlugin("com.indoorvivants" % "sbt-commandmatrix" % "0.1.0")
addSbtPlugin("org.scala-js" % "sbt-scalajs" % "1.22.0")
addSbtPlugin("org.scala-native" % "sbt-scala-native" % "0.5.12")
// publishing
addSbtPlugin("com.github.sbt" % "sbt-pgp" % "2.3.1")
// MiMa
addSbtPlugin("com.typesafe" % "sbt-mima-plugin" % "1.1.6")
// benchmarks
addSbtPlugin("pl.project13.scala" % "sbt-jmh" % "0.4.8")
// disabling projects in IDE
addSbtPlugin("org.jetbrains.scala" % "sbt-ide-settings" % "1.1.4")
// testing protobufs
// NOTE: sbt-protoc has no stable sbt 2 release yet - 1.1.0-RC2 is the only sbt 2 build. It pulls
// protoc-bridge_3, which on the scalapb side only lines up with the 1.0.0-alpha line: stable scalapb
// (0.11.20) still resolves the protoc-gen_2.13/protoc-bridge_2.13 chain, and mixing that with sbt-protoc's
// protoc-bridge_3 fails sbt's cross-version-suffix check. So the sbt 2 migration forces scalapb 1.0.0-alpha.6
// for BOTH the codegen (here) and the runtime (versions.scalapbRuntime derives from it). See build.sbt.
addSbtPlugin("com.thesamet" % "sbt-protoc" % "1.1.0-RC2")
libraryDependencies += "com.thesamet.scalapb" %% "compilerplugin" % "1.0.0-alpha.6"
// documentation
addSbtPlugin("com.github.reibitto" % "sbt-welcome" % "0.6.0")

ThisBuild / libraryDependencySchemes += "org.scala-lang.modules" %% "scala-xml" % VersionScheme.Always
