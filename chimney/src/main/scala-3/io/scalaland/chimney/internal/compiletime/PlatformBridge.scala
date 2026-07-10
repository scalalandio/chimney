package io.scalaland.chimney.internal.compiletime

import scala.quoted.Quotes

/** Scala 3 entrypoint of the macro cake: concrete macro classes extend this class. It also hosts the Scala 3 overrides
  * of the compat workarounds (see [[MacroCommonsCompat]]).
  */
abstract private[compiletime] class PlatformBridge(q: Quotes)
    extends hearth.MacroCommonsScala3(using q)
    with ChimneyDefinitions
// NOTE: the `cacheScopeToken` override is GONE: Hearth's `Type.Cache` partitions by the active Cross-Quotes scope
// itself since hearth#347 (the same splice-scoping Chimney's removed `TypeCache` implemented).
