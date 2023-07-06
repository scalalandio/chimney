package io.scalaland.chimney.internal.compiletime.derivation.patcher

import io.scalaland.chimney.internal.compiletime.{datatypes, ChimneyDefinitionsPlatform}
import io.scalaland.chimney.internal.compiletime.derivation.transformer

abstract private[compiletime] class DerivationPlatform(q: scala.quoted.Quotes)
    extends transformer.DerivationPlatform(q)
    with Derivation
