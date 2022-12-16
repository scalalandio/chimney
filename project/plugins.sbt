addSbtPlugin("org.scoverage" % "sbt-scoverage" % "2.0.5")
addSbtPlugin("org.scala-js" % "sbt-scalajs" % "1.10.1")
addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "1.2.0")

addSbtPlugin("org.scalameta" % "sbt-scalafmt" % "2.4.6")

addSbtPlugin("com.typesafe.sbt" % "sbt-site" % "1.4.0")
addSbtPlugin("com.typesafe.sbt" % "sbt-ghpages" % "0.6.3")

addSbtPlugin("org.xerial.sbt" % "sbt-sonatype" % "3.9.13")

addSbtPlugin("pl.project13.scala" % "sbt-jmh" % "0.4.3")

ThisBuild / libraryDependencySchemes += "org.scala-lang.modules" %% "scala-xml" % VersionScheme.Always
