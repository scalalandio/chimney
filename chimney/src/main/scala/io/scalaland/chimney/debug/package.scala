package io.scalaland.chimney

package object debug {

  /** Import this value to enable derivation debug logging for all Chimney macros expanded in the file:
    *
    * {{{
    * import io.scalaland.chimney.debug._
    * }}}
    *
    * The macro prints the matched rules, summoned implicits and the generated code to the compiler output - the same
    * information `.enableMacrosLogging` shows, but without touching the transformation definition.
    *
    * Alternatively, enable it project-wide with scalac option: `-Xmacro-settings:chimney.logDerivation=true`.
    *
    * @since 2.0.0
    */
  implicit val logDerivationForChimney: LogDerivation = new LogDerivation
}
