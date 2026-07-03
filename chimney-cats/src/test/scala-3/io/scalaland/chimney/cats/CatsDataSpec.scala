package io.scalaland.chimney.cats

import io.scalaland.chimney.ChimneySpec
import io.scalaland.chimney.dsl.*

/** Scala 3 twin of the scala-2 CatsDataSpec - pins the CURRENT GAP instead of the conversions.
  *
  * Since 2.0.0 `chimney-cats` ships NO `io.scalaland.chimney.integrations.*` implicits for Cats collections
  * (`CatsDataImplicits` was removed) - Cats collection conversions are served by the `StandardMacroExtension`s of
  * `com.kubuszok::kindlings-cats-integration` put on the compile classpath instead (see the scala-2 spec).
  *
  * On Scala 3 that dependency is currently ABSENT: kindlings publishes only Scala-3.8.4-built artifacts (TASTy 28.8),
  * which chimney's Scala 3.7.3 cannot load - hearth's `ensureStandardExtensionsLoaded` throws "Forward incompatible
  * TASTy file" for EVERY derivation in a module that has such an extension on the classpath (verified empirically).
  * Until kindlings publishes 3.3-LTS-built artifacts (like hearth itself does) or chimney bumps to Scala 3.8+, Cats
  * collection conversions are NOT available on Scala 3, which this spec pins.
  */
class CatsDataSpec extends ChimneySpec {

  test("GAP (Scala 3): cats collection conversions unavailable - kindlings extension not loadable on Scala 3.7") {
    compileErrors("""List("test").transformInto[_root_.cats.data.Chain[String]]""").check(
      "Chimney can't derive transformation from"
    )
    compileErrors("""List("test").transformIntoPartial[_root_.cats.data.NonEmptyList[String]]""").check(
      "Chimney can't derive transformation from"
    )
    compileErrors("""_root_.cats.data.NonEmptyList.one("test").transformInto[List[String]]""").check(
      "Chimney can't derive transformation from"
    )
  }
}
