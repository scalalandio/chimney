package io.scalaland.chimney.internal.compiletime

abstract class DerivationEnginePlatform(q: scala.quoted.Quotes)
    extends ChimneyDefinitionsPlatform(q)
    with DerivationEngine
    with datatypes.IterableOrArraysPlatform
    with datatypes.ProductTypesPlatform
    with datatypes.SealedHierarchiesPlatform
    with datatypes.ValueClassesPlatform
