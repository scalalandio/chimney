package io.scalaland.chimney.internal.compiletime2

import scala.quoted.Quotes

/** Scala 3 entrypoint of the Hearth-based macro cake.
  *
  * Mirrors the old `DefinitionsPlatform`/`DerivationPlatform` split: concrete macro classes will extend this class the
  * same way they extended `DefinitionsPlatform(using q)` before. Its main purpose right now is to prove that the whole
  * `compiletime2` cake composes and compiles on Scala 3.
  */
private[compiletime2] abstract class PlatformBridge(q: Quotes)
    extends hearth.MacroCommonsScala3(using q)
    with ChimneyDefinitions
