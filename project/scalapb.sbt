addSbtPlugin("com.thesamet" % "sbt-protoc" % "0.99.8" exclude ("com.trueaccord.scalapb", "protoc-bridge_2.10"))

libraryDependencies += "com.trueaccord.scalapb" %% "compilerplugin-shaded" % "0.6.0-pre4"
