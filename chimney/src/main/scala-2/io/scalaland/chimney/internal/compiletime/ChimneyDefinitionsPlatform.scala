package io.scalaland.chimney.internal.compiletime

private[compiletime] trait ChimneyDefinitionsPlatform
    extends ChimneyDefinitions
    with DefinitionsPlatform
    with ChimneyTypesPlatform
    with ChimneyExprsPlatform
