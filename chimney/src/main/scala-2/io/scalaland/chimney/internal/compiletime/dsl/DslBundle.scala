package io.scalaland.chimney.internal.compiletime.dsl

import io.scalaland.chimney.internal.compiletime.PlatformBridge
import io.scalaland.chimney.internal.runtime

import scala.reflect.macros.whitebox

/** Scala 2 entrypoint of the DSL macros: whitebox bundles extend this class and forward to the shared [[DslMacros]]
  * bodies (`whitebox.Context <: blackbox.Context`, so it can be passed up to [[PlatformBridge]]).
  */
abstract private[compiletime] class DslBundle(ctx: whitebox.Context) extends PlatformBridge(ctx) with DslMacros {

  protected def prefixExpr: Expr[runtime.WithRuntimeDataStore] =
    c.Expr[runtime.WithRuntimeDataStore](c.prefix.tree)

  protected def prefixAnyExpr: Expr[Any] = c.Expr[Any](c.prefix.tree)

  protected def anyExpr(tree: c.Tree): Expr[Any] = c.Expr[Any](tree)
}
