package io.scalaland.chimney

/** Base trait of all Chimney specs: Hearth's MacroSuite (`group`, `==>`, `compileErrors(...).check/checkNot`,
  * `ignoreOnScala2_13`/`ignoreOnScala3` etc.) + Chimney's own cross-version helpers.
  */
trait ChimneySpec extends hearth.MacroSuite with VersionCompat
