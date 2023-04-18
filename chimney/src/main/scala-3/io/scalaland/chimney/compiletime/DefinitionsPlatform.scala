package io.scalaland.chimney.internal.compiletime

import io.scalaland.chimney.dsl as dsls
import io.scalaland.chimney.internal
import io.scalaland.chimney.{partial, PartialTransformer, Patcher, Transformer}

import scala.quoted

private[compiletime] trait DefinitionsPlatform(using val quotes: quoted.Quotes)
    extends Definitions
    with TypesPlatform
    with ChimneyTypesPlatform
    with ExprsPlatform
    with ChimneyExprsPlatform
    with ConfigurationsPlatform
    with ResultsPlatform
