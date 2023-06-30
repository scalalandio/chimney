package io.scalaland.chimney.internal.compiletime

import scala.reflect.macros.blackbox

trait DefinitionsPlatform
    extends Definitions
    with TypesPlatform
    with ExprsPlatform
    with ExprPromisesPlatform
    with ResultsPlatform {

  val c: blackbox.Context
}
