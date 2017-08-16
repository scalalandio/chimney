resolvers += Resolver.sonatypeRepo("snapshots")

addSbtPlugin("com.thesamet" % "sbt-protoc" % "0.99.12-rc5")
libraryDependencies += "com.trueaccord.scalapb" %% "compilerplugin-shaded" % "0.6.1"

addSbtPlugin("org.scoverage" % "sbt-scoverage" % "1.5.1")
addSbtPlugin("org.scala-js" % "sbt-scalajs" % "0.6.20")

addSbtPlugin("com.geirsson" % "sbt-scalafmt" % "1.2.0")
