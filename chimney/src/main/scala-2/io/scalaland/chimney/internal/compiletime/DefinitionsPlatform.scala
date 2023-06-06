package io.scalaland.chimney.internal.compiletime

import scala.reflect.macros.blackbox

private[compiletime] trait DefinitionsPlatform
    extends Definitions
    with TypesPlatform
    with ChimneyTypesPlatform
    with ExprsPlatform
    with ExprPromisesPlatform
    with ChimneyExprsPlatform
    with ResultsPlatform {

  val c: blackbox.Context
}
