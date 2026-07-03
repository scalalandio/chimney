package io.scalaland.chimney.internal

import hearth.fp.effect.{MErrors, MIO}

/** `DerivationResult`/`DerivationErrors` are aliases for Hearth's
  * [[hearth.fp.effect.MIO]]/[[hearth.fp.effect.MErrors]]. Their combinators are provided by
  * [[compiletime.ResultSyntax]], mixed into `ChimneyDefinitions` rather than defined here - package-object implicits
  * would NOT be visible in nested packages (verified experimentally).
  */
package object compiletime {

  private[compiletime] type DerivationResult[+A] = MIO[A]

  private[compiletime] type DerivationErrors = MErrors
}
