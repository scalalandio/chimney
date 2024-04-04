package io.scalaland.chimney.internal.compiletime.derivation.patcher

import io.scalaland.chimney.{dsl, Patcher}
import io.scalaland.chimney.internal.runtime

import scala.reflect.macros.blackbox

final class PatcherMacros(val c: blackbox.Context) extends DerivationPlatform with Gateway {

  import c.universe.{internal as _, Transformer as _, *}

  def derivePatchWithConfig[
      A: WeakTypeTag,
      Patch: WeakTypeTag,
      Tail <: runtime.PatcherOverrides: WeakTypeTag,
      Flags <: runtime.PatcherFlags: WeakTypeTag,
      ImplicitScopeFlags <: runtime.PatcherFlags: WeakTypeTag
  ](
      tc: Expr[io.scalaland.chimney.dsl.PatcherConfiguration[ImplicitScopeFlags]]
  ): c.Expr[A] = retypecheck(
    // Called by PatcherUsing => prefix is PatcherUsing
    cacheDefinition(c.Expr[dsl.PatcherUsing[A, Patch, Tail, Flags]](c.prefix.tree)) { pu =>
      Expr.block(
        List(Expr.suppressUnused(pu)),
        derivePatcherResult[A, Patch, Tail, Flags, ImplicitScopeFlags](
          obj = c.Expr[A](q"$pu.obj"),
          patch = c.Expr[Patch](q"$pu.objPatch")
        )
      )
    }
  )

  def derivePatcherWithConfig[
      A: WeakTypeTag,
      Patch: WeakTypeTag,
      Tail <: runtime.PatcherOverrides: WeakTypeTag,
      InstanceFlags <: runtime.PatcherFlags: WeakTypeTag,
      ImplicitScopeFlags <: runtime.PatcherFlags: WeakTypeTag
  ](
      tc: Expr[io.scalaland.chimney.dsl.PatcherConfiguration[ImplicitScopeFlags]]
  ): Expr[Patcher[A, Patch]] = retypecheck(
    cacheDefinition(c.Expr[dsl.PatcherDefinition[A, Patch, Tail, InstanceFlags]](c.prefix.tree)) { pu =>
      Expr.block(
        List(Expr.suppressUnused(pu)),
        derivePatcher[A, Patch, Tail, InstanceFlags, ImplicitScopeFlags]
      )
    }
  )

  def derivePatcherWithDefaults[
      A: WeakTypeTag,
      Patch: WeakTypeTag
  ]: Expr[Patcher[A, Patch]] = retypecheck(
    resolveImplicitScopeConfigAndMuteUnusedWarnings { implicitScopeFlagsType =>
      import implicitScopeFlagsType.Underlying as ImplicitScopeFlags
      derivePatcher[A, Patch, runtime.PatcherOverrides.Empty, runtime.PatcherFlags.Default, ImplicitScopeFlags]
    }
  )

  private def resolveImplicitScopeConfigAndMuteUnusedWarnings[A: Type](
      useImplicitScopeFlags: ?<[runtime.PatcherFlags] => Expr[A]
  ): Expr[A] = {
    val implicitScopeConfig = {
      import c.universe.*

      val searchTypeTree =
        tq"${typeOf[io.scalaland.chimney.dsl.PatcherConfiguration[? <: runtime.PatcherFlags]]}"
      val typedTpeTree = c.typecheck(
        tree = searchTypeTree,
        silent = true,
        mode = c.TYPEmode,
        withImplicitViewsDisabled = true,
        withMacrosDisabled = false
      )

      scala.util
        .Try(c.inferImplicitValue(typedTpeTree.tpe, silent = true, withMacrosDisabled = false))
        .toOption
        .filterNot(_ == c.universe.EmptyTree)
        .getOrElse {
          // $COVERAGE-OFF$
          reportError("Can't locate implicit PatcherConfiguration!")
          // $COVERAGE-ON$
        }
    }
    val implicitScopeFlagsType = Type.platformSpecific
      .fromUntyped[runtime.PatcherFlags](implicitScopeConfig.tpe.typeArgs.head)
      .as_?<[runtime.PatcherFlags]

    Expr.block(
      List(Expr.suppressUnused(c.Expr(implicitScopeConfig)(implicitScopeFlagsType.Underlying))),
      useImplicitScopeFlags(implicitScopeFlagsType)
    )
  }

  private def retypecheck[A: Type](expr: c.Expr[A]): c.Expr[A] = try
    c.Expr[A](c.typecheck(tree = c.untypecheck(expr.tree)))
  catch {
    case scala.reflect.macros.TypecheckException(_, msg) => c.abort(c.enclosingPosition, msg)
  }
}
