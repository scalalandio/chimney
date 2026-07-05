package io.scalaland.chimney.internal.compiletime

import scala.quoted.Quotes

/** Scala 3 entrypoint of the macro cake: concrete macro classes extend this class. It also hosts the Scala 3 overrides
  * of the compat workarounds (see [[MacroCommonsCompat]]).
  */
abstract private[compiletime] class PlatformBridge(q: Quotes)
    extends hearth.MacroCommonsScala3(using q)
    with ChimneyDefinitions {

  /** Scala 3 override of [[MacroCommonsCompat.cacheScopeToken]]: the ACTIVE Cross-Quotes `Quotes`. Each `Expr.splice`
    * evaluates its thunks under a fresh nested `Quotes`, so values a `TypeCache` materializes during one splice are
    * never handed out during another (the cross-quotes usage contract; Iso/Codec derive two instances - two sibling
    * splices - per expansion).
    */
  override protected def cacheScopeToken: AnyRef = CrossQuotes.ctx[scala.quoted.Quotes]
}
