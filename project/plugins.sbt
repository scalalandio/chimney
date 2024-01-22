// git
addSbtPlugin("com.github.sbt" % "sbt-git" % "2.0.1")
// linters
addSbtPlugin("org.scalameta" % "sbt-scalafmt" % "2.5.2")
addSbtPlugin("org.scoverage" % "sbt-scoverage" % "2.0.9")
// cross-compile
addSbtPlugin("com.eed3si9n" % "sbt-projectmatrix" % "0.9.2")
addSbtPlugin("com.indoorvivants" % "sbt-commandmatrix" % "0.0.5")
addSbtPlugin("org.scala-js" % "sbt-scalajs" % "1.15.0")
addSbtPlugin("org.scala-native" % "sbt-scala-native" % "0.4.17")
// publishing
addSbtPlugin("org.xerial.sbt" % "sbt-sonatype" % "3.10.0")
addSbtPlugin("com.github.sbt" % "sbt-pgp" % "2.2.1")
// MiMa
addSbtPlugin("com.typesafe" % "sbt-mima-plugin" % "1.1.3")
// benchmarks
addSbtPlugin("pl.project13.scala" % "sbt-jmh" % "0.4.7")
// disabling projects in IDE
addSbtPlugin("org.jetbrains" % "sbt-ide-settings" % "1.1.0")
// testing protobufs
addSbtPlugin("com.thesamet" % "sbt-protoc" % "1.0.6")
libraryDependencies += "com.thesamet.scalapb" %% "compilerplugin" % "0.11.15"
//
addSbtPlugin("com.github.reibitto" % "sbt-welcome" % "0.4.0")

ThisBuild / libraryDependencySchemes += "org.scala-lang.modules" %% "scala-xml" % VersionScheme.Always
