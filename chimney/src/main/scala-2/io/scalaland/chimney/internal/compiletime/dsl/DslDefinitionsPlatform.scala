package io.scalaland.chimney.internal.compiletime.dsl

import io.scalaland.chimney.internal.compiletime.ChimneyDefinitionsPlatform

private[chimney] trait DslDefinitionsPlatform
    extends DslDefinitions
    with ChimneyDefinitionsPlatform
    with TypeEncodingsPlatform
