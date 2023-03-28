package io.scalaland.chimney.internal.compiletime

import scala.reflect.macros.blackbox

private[compiletime] trait DefinitionsPlatform
    extends Definitions
    with TypesPlatform
    with ExprsPlatform
    with ConfigurationsPlatform
    with ResultsPlatform {

  val c: blackbox.Context
}
private[compiletime] object DefinitionsPlatform {
  type Arbitrary
  type Arbitrary2
}
