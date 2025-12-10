// git
addSbtPlugin("com.github.sbt" % "sbt-git" % "2.1.0")
// linters
addSbtPlugin("org.scalameta" % "sbt-scalafmt" % "2.5.6")
addSbtPlugin("org.scoverage" % "sbt-scoverage" % "2.4.3")
// cross-compile
addSbtPlugin("com.eed3si9n" % "sbt-projectmatrix" % "0.11.0")
addSbtPlugin("com.indoorvivants" % "sbt-commandmatrix" % "0.0.5")
addSbtPlugin("org.scala-js" % "sbt-scalajs" % "1.20.1")
addSbtPlugin("org.scala-native" % "sbt-scala-native" % "0.5.9")
// publishing
addSbtPlugin("com.github.sbt" % "sbt-pgp" % "2.3.1")
// MiMa
addSbtPlugin("com.typesafe" % "sbt-mima-plugin" % "1.1.4")
// benchmarks
addSbtPlugin("pl.project13.scala" % "sbt-jmh" % "0.4.8")
// disabling projects in IDE
addSbtPlugin("org.jetbrains" % "sbt-ide-settings" % "1.1.0")
// testing protobufs
addSbtPlugin("com.thesamet" % "sbt-protoc" % "1.0.8")
libraryDependencies += "com.thesamet.scalapb" %% "compilerplugin" % "0.11.20"
// documentation
addSbtPlugin("com.github.reibitto" % "sbt-welcome" % "0.5.0")

ThisBuild / libraryDependencySchemes += "org.scala-lang.modules" %% "scala-xml" % VersionScheme.Always
