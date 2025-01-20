package io.scalaland.chimney.internal.compiletime.derivation.patcher

import io.scalaland.chimney.{dsl, Patcher}
import io.scalaland.chimney.internal.runtime

import scala.reflect.macros.blackbox

final class PatcherMacros(val c: blackbox.Context) extends DerivationPlatform with Gateway {

  import c.universe.{internal as _, Transformer as _, *}

  def derivePatchWithConfig[
      A: WeakTypeTag,
      Patch: WeakTypeTag,
      Overrides <: runtime.PatcherOverrides: WeakTypeTag,
      Flags <: runtime.PatcherFlags: WeakTypeTag,
      ImplicitScopeFlags <: runtime.PatcherFlags: WeakTypeTag
  ](
      tc: Expr[io.scalaland.chimney.dsl.PatcherConfiguration[ImplicitScopeFlags]]
  ): c.Expr[A] = retypecheck(
    // Called by PatcherUsing => prefix is PatcherUsing
    cacheDefinition(c.Expr[dsl.PatcherUsing[A, Patch, Overrides, Flags]](c.prefix.tree)) { pu =>
      Expr.block(
        List(Expr.suppressUnused(tc)),
        derivePatcherResult[A, Patch, Overrides, Flags, ImplicitScopeFlags](
          obj = c.Expr[A](q"$pu.obj"),
          patch = c.Expr[Patch](q"$pu.objPatch")
        )
      )
    }
  )

  def derivePatcherWithConfig[
      A: WeakTypeTag,
      Patch: WeakTypeTag,
      Overrides <: runtime.PatcherOverrides: WeakTypeTag,
      InstanceFlags <: runtime.PatcherFlags: WeakTypeTag,
      ImplicitScopeFlags <: runtime.PatcherFlags: WeakTypeTag
  ](
      tc: Expr[io.scalaland.chimney.dsl.PatcherConfiguration[ImplicitScopeFlags]]
  ): Expr[Patcher[A, Patch]] = retypecheck(
    Expr.block(
      List(Expr.suppressUnused(tc)),
      derivePatcher[A, Patch, Overrides, InstanceFlags, ImplicitScopeFlags]
    )
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
      val patcherConfigurationType = Type.platformSpecific
        .fromUntyped[io.scalaland.chimney.dsl.PatcherConfiguration[? <: runtime.PatcherFlags]](
          c.typecheck(
            tree = tq"${typeOf[io.scalaland.chimney.dsl.PatcherConfiguration[? <: runtime.PatcherFlags]]}",
            silent = true,
            mode = c.TYPEmode,
            withImplicitViewsDisabled = true,
            withMacrosDisabled = false
          ).tpe
        )

      Expr.summonImplicit(patcherConfigurationType).getOrElse {
        // $COVERAGE-OFF$should never happen unless someone mess around with type-level representation
        reportError("Can't locate implicit PatcherConfiguration!")
        // $COVERAGE-ON$
      }
    }
    val implicitScopeFlagsType = Type.platformSpecific
      .fromUntyped[runtime.PatcherFlags](implicitScopeConfig.tpe.tpe.typeArgs.head)
      .as_?<[runtime.PatcherFlags]

    Expr.block(
      List(Expr.suppressUnused(implicitScopeConfig)),
      useImplicitScopeFlags(implicitScopeFlagsType)
    )
  }

  private def retypecheck[A: Type](expr: c.Expr[A]): c.Expr[A] = try
    c.Expr[A](c.typecheck(tree = c.untypecheck(expr.tree)))
  catch {
    case scala.reflect.macros.TypecheckException(_, msg) => c.abort(c.enclosingPosition, msg)
  }
}
