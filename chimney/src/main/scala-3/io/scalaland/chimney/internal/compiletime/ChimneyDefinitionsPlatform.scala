package io.scalaland.chimney.internal.compiletime

abstract private[compiletime] class ChimneyDefinitionsPlatform(q: scala.quoted.Quotes)
    extends DefinitionsPlatform(using q)
    with ChimneyDefinitions
    with ChimneyTypesPlatform
    with ChimneyExprsPlatform
