addSbtPlugin("com.thesamet" % "sbt-protoc" % "0.99.8" exclude ("com.trueaccord.scalapb", "protoc-bridge_2.10"))
libraryDependencies += "com.trueaccord.scalapb" %% "compilerplugin-shaded" % "0.6.1"

addSbtPlugin("io.get-coursier" % "sbt-coursier" % "1.0.0-RC3")
addSbtPlugin("org.scoverage" % "sbt-scoverage" % "1.5.0")
addSbtPlugin("org.scala-js" % "sbt-scalajs" % "0.6.18")

libraryDependencies += "com.geirsson" %% "scalafmt-bootstrap" % "0.6.6"
