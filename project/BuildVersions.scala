import sbt.*
import sbt.librarymanagement.CrossVersion

// Build-level version/axis config (`versions`) and IDE dev settings (`dev`). These live in project/*.scala rather
// than build.sbt because sbt 2.x compiles build.sbt with Scala 3, which (a) widens a build.sbt-local
// `val x = new { ... }` to `Object` and hides its members, and (b) does not bring a build.sbt-local `object`/`class`
// into the settings scope. As real objects on the meta-build classpath they are visible in build.sbt unqualified
// (`versions.scala3`, `dev.idePlatform`, ...), exactly as before the sbt 2 migration.

object versions {
  // Versions we are publishing for.
  val scala213 = "2.13.18"
  val scala3 = "3.8.4"
  // For chimney-sandwich-test-cases-3 ONLY: sbt forbids Scala 2.13 subprojects from depending on Scala 3.8+
  // subprojects (sbt-8728), and 2.13's -Ytasty-reader tops out below TASTy 28.8 - see the module for details.
  val scala3Sandwich = "3.7.3"

  // Which versions should be cross-compiled for publishing
  val scalas = List(scala213, scala3)

  val platforms = List(VirtualAxis.jvm, VirtualAxis.js, VirtualAxis.native)

  // Dependencies.
  val hearth = "0.4.1"
  val cats = "2.13.0"
  // kindlings 0.3.1 is the first release on hearth 0.4.1 and includes kubuszok/kindlings#163
  // (NonEmptySeq/NonEmptyLazyList IsCollection providers).
  val kindlingsCatsIntegration = "0.3.1"
  val kindProjector = "0.13.4"
  val munit = "1.3.4"
  val scalaCollectionCompat = "2.14.0"
  val scalaJavaCompat = "1.0.2"
  val scalaJavaTime = "2.7.0"
  val scalapbRuntime = scalapb.compiler.Version.scalapbVersion

  // Explicitly handle Scala 2.13 and Scala 3 separately.
  def fold[A](scalaVersion: String)(for2_13: => Seq[A], for3: => Seq[A]): Seq[A] =
    CrossVersion.partialVersion(scalaVersion) match {
      case Some((2, 13)) => for2_13
      case Some((3, _))  => for3
      case _             => Seq.empty // for sbt
    }
}

object dev {

  val props = scala.util
    .Using(new java.io.FileInputStream("dev.properties")) { fis =>
      val props = new java.util.Properties()
      props.load(fis)
      props
    }
    .get

  // Which version should be used in IntelliJ
  val ideScala = props.getProperty("ide.scala") match {
    case "2.13" => versions.scala213
    case "3"    => versions.scala3
  }
  val idePlatform = props.getProperty("ide.platform") match {
    case "jvm"    => VirtualAxis.jvm
    case "js"     => VirtualAxis.js
    case "native" => VirtualAxis.native
  }

  def isIdeScala(scalaVersion: String): Boolean =
    CrossVersion.partialVersion(scalaVersion) == CrossVersion.partialVersion(ideScala)
  def isIdePlatform(platform: VirtualAxis): Boolean = platform == idePlatform
}
