addSbtPlugin("com.thesamet" % "sbt-protoc" % "0.99.8" exclude ("com.trueaccord.scalapb", "protoc-bridge_2.10"))
libraryDependencies += "com.trueaccord.scalapb" %% "compilerplugin-shaded" % "0.6.0-pre4"

addSbtPlugin("org.scoverage" % "sbt-scoverage" % "1.5.0")
addSbtPlugin("org.scala-js" % "sbt-scalajs" % "0.6.17")
addSbtPlugin("com.lucidchart" % "sbt-scalafmt" % "0.3")