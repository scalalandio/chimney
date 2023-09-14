// git
addSbtPlugin("com.typesafe.sbt" % "sbt-git" % "1.0.2")
// linters
addSbtPlugin("org.scalameta" % "sbt-scalafmt" % "2.4.6")
addSbtPlugin("org.scoverage" % "sbt-scoverage" % "2.0.8")
// cross-compile
addSbtPlugin("com.eed3si9n" % "sbt-projectmatrix" % "0.9.1")
addSbtPlugin("com.indoorvivants" % "sbt-commandmatrix" % "0.0.5")
addSbtPlugin("org.scala-js" % "sbt-scalajs" % "1.13.2")
addSbtPlugin("org.scala-native" % "sbt-scala-native" % "0.4.14")
// publishing
addSbtPlugin("org.xerial.sbt" % "sbt-sonatype" % "3.9.21")
// MiMa
addSbtPlugin("com.typesafe" % "sbt-mima-plugin" % "1.1.2")
// benchmarks
addSbtPlugin("pl.project13.scala" % "sbt-jmh" % "0.4.6")
// disabling projects in IDE
addSbtPlugin("org.jetbrains" % "sbt-ide-settings" % "1.1.0")
// testing protobufs
addSbtPlugin("com.thesamet" % "sbt-protoc" % "1.0.6")
libraryDependencies += "com.thesamet.scalapb" %% "compilerplugin" % "0.11.13"
//
addSbtPlugin("com.github.reibitto" % "sbt-welcome" % "0.3.1")

ThisBuild / libraryDependencySchemes += "org.scala-lang.modules" %% "scala-xml" % VersionScheme.Always
