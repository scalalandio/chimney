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

  /** Java-enum values selected with `.withSealedSubtypeHandled(...)` need their singleton type refined into
    * `runtime.RefinedJavaEnum[E, "Name"]` (see `MacroCommonsCompat.fixJavaEnumCompat`). The singleton is only present
    * SYNTACTICALLY - in the closure parameter's `TypeTree.original` (`SingletonTypeTree`) - which typed expressions
    * (and thus `DestructuredExpr`) do not preserve, hence this stays platform-side.
    */
  protected def javaEnumFixedSubtype[Subtype](f: c.Tree)(implicit subtype: Type[Subtype]): ?? = {
    import c.universe.*
    if (subtype.tpe.typeSymbol.isJavaEnum) {
      val Function(List(ValDef(_, _, lhs: TypeTree, _)), _) = f: @unchecked
      lhs.original match {
        case SingletonTypeTree(Literal(Constant(t: TermSymbol))) =>
          val instanceName = c.internal.constantType(Constant(t.name.decodedName.toString))
          c.WeakTypeTag[Any](
            appliedType(
              typeOf[io.scalaland.chimney.internal.runtime.RefinedJavaEnum[?, ?]].typeConstructor,
              subtype.tpe,
              instanceName
            )
          ).as_??
        case _ => subtype.as_??
      }
    } else subtype.as_??
  }
}
