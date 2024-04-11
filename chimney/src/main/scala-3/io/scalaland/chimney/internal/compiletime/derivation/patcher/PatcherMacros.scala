package io.scalaland.chimney.internal.compiletime.derivation.patcher

import io.scalaland.chimney.Patcher
import io.scalaland.chimney.internal.runtime

import scala.quoted.{Expr, Quotes, Type}

final class PatcherMacros(q: Quotes) extends DerivationPlatform(q) with Gateway {

  import quotes.*, quotes.reflect.*

  def derivePatcherWithConfig[
      A: Type,
      Patch: Type,
      Overrides <: runtime.PatcherOverrides: Type,
      Flags <: runtime.PatcherFlags: Type,
      ImplicitScopeFlags <: runtime.PatcherFlags: Type
  ]: Expr[Patcher[A, Patch]] = suppressWarnings {
    derivePatcher[A, Patch, Overrides, Flags, ImplicitScopeFlags]
  }

  def derivePatcherWithDefaults[
      A: Type,
      Patch: Type
  ]: Expr[Patcher[A, Patch]] = suppressWarnings {
    resolveImplicitScopeConfigAndMuteUnusedWarnings { implicitScopeFlagsType =>
      import implicitScopeFlagsType.Underlying as ImplicitScopeFlags
      derivePatcher[A, Patch, runtime.PatcherOverrides.Empty, runtime.PatcherFlags.Default, ImplicitScopeFlags]
    }
  }

  private def resolveImplicitScopeConfigAndMuteUnusedWarnings[A: Type](
      useImplicitScopeFlags: ?<[runtime.PatcherFlags] => Expr[A]
  ): Expr[A] = {
    val implicitScopeConfig = scala.quoted.Expr
      .summon[io.scalaland.chimney.dsl.PatcherConfiguration[? <: runtime.PatcherFlags]]
      .getOrElse {
        // $COVERAGE-OFF$
        reportError("Can't locate implicit PatcherConfiguration!")
        // $COVERAGE-ON$
      }
    val implicitScopeFlagsType = implicitScopeConfig.asTerm.tpe.widen.typeArgs.head.asType
      .asInstanceOf[Type[runtime.PatcherFlags]]
      .as_?<[runtime.PatcherFlags]

    Expr.block(
      List(Expr.suppressUnused(implicitScopeConfig)),
      useImplicitScopeFlags(implicitScopeFlagsType)
    )
  }

  private def suppressWarnings[A: Type](expr: Expr[A]): Expr[A] = '{
    @SuppressWarnings(Array("org.wartremover.warts.All", "all"))
    val result = ${ expr }
    result
  }
}

object PatcherMacros {

  final def derivePatcherWithConfig[
      A: Type,
      Patch: Type,
      Overrides <: runtime.PatcherOverrides: Type,
      Flags <: runtime.PatcherFlags: Type,
      ImplicitScopeFlags <: runtime.PatcherFlags: Type
  ](using q: Quotes): Expr[Patcher[A, Patch]] =
    new PatcherMacros(q).derivePatcherWithConfig[A, Patch, Overrides, Flags, ImplicitScopeFlags]

  final def derivePatcherWithDefaults[A: Type, Patch: Type](using q: Quotes): Expr[Patcher[A, Patch]] =
    new PatcherMacros(q).derivePatcherWithDefaults[A, Patch]

  final def derivePatcherResultWithConfig[
      A: Type,
      Patch: Type,
      Overrides <: runtime.PatcherOverrides: Type,
      Flags <: runtime.PatcherFlags: Type,
      ImplicitScopeFlags <: runtime.PatcherFlags: Type
  ](
      obj: Expr[A],
      patch: Expr[Patch]
  )(using q: Quotes): Expr[A] =
    new PatcherMacros(q).derivePatcherResult[A, Patch, Overrides, Flags, ImplicitScopeFlags](obj, patch)
}
