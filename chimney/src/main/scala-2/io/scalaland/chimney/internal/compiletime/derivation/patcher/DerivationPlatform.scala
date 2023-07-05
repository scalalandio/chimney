package io.scalaland.chimney.internal.compiletime.derivation.patcher

import io.scalaland.chimney.internal.compiletime.{datatypes, ChimneyDefinitionsPlatform}

trait DerivationPlatform
    extends Derivation
    with ChimneyDefinitionsPlatform
    with datatypes.ProductTypesPlatform
    with datatypes.SealedHierarchiesPlatform
    with datatypes.ValueClassesPlatform
