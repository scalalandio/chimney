package io.scalaland.chimney.internal.compiletime

import scala.reflect.macros.blackbox

trait DefinitionsPlatform
    extends Definitions
    with TypesPlatform
    with ExprsPlatform
    with ExprPromisesPlatform
    with ResultsPlatform {

  val c: blackbox.Context

  protected val isScala212 = scala.util.Properties.versionNumberString < "2.13"
}
