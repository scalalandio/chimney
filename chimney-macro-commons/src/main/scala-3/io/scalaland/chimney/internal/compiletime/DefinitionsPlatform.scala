package io.scalaland.chimney.internal.compiletime

import scala.quoted

abstract class DefinitionsPlatform(using val quotes: quoted.Quotes)
    extends Definitions
    with TypesPlatform
    with ExprsPlatform
    with ExprPromisesPlatform
    with ResultsPlatform
