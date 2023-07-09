package io.scalaland.chimney.internal.compiletime.dsl

import io.scalaland.chimney.internal.compiletime.ChimneyDefinitionsPlatform

abstract private[chimney] class DslDefinitionsPlatform(q: scala.quoted.Quotes)
    extends ChimneyDefinitionsPlatform(q)
    with DslDefinitions
    with TypeEncodingsPlatform
