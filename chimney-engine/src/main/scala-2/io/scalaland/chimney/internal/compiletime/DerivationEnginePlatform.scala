package io.scalaland.chimney.internal.compiletime

trait DerivationEnginePlatform
    extends DerivationEngine
    with ChimneyDefinitionsPlatform
    with datatypes.IterableOrArraysPlatform
    with datatypes.ProductTypesPlatform
    with datatypes.SealedHierarchiesPlatform
    with datatypes.ValueClassesPlatform
