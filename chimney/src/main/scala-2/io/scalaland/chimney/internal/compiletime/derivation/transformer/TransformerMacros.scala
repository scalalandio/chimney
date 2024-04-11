package io.scalaland.chimney.internal.compiletime.derivation.transformer

import io.scalaland.chimney.dsl
import io.scalaland.chimney.{PartialTransformer, Transformer}
import io.scalaland.chimney.internal.runtime
import io.scalaland.chimney.partial

import scala.reflect.macros.blackbox

final class TransformerMacros(val c: blackbox.Context) extends DerivationPlatform with Gateway {

  import c.universe.{internal as _, Transformer as _, *}

  def deriveTotalTransformationWithConfig[
      From: WeakTypeTag,
      To: WeakTypeTag,
      Tail <: runtime.TransformerOverrides: WeakTypeTag,
      InstanceFlags <: runtime.TransformerFlags: WeakTypeTag,
      ImplicitScopeFlags <: runtime.TransformerFlags: WeakTypeTag
  ](
      tc: Expr[io.scalaland.chimney.dsl.TransformerConfiguration[ImplicitScopeFlags]]
  ): Expr[To] = retypecheck(
    suppressWarnings(
      // Called by TransformerInto => prefix is TransformerInto
      // We're caching it because it is used twice: once for RuntimeDataStore and once for source
      cacheDefinition(c.Expr[dsl.TransformerInto[From, To, Tail, InstanceFlags]](c.prefix.tree)) { ti =>
        Expr.block(
          List(Expr.suppressUnused(tc)),
          deriveTotalTransformationResult[From, To, Tail, InstanceFlags, ImplicitScopeFlags](
            src = c.Expr[From](q"$ti.source"),
            runtimeDataStore = c.Expr[dsl.TransformerDefinitionCommons.RuntimeDataStore](q"$ti.td.runtimeData")
          )
        )
      }
    )
  )

  def deriveTotalTransformerWithDefaults[
      From: WeakTypeTag,
      To: WeakTypeTag
  ]: Expr[Transformer[From, To]] = retypecheck(
    suppressWarnings(
      resolveImplicitScopeConfigAndMuteUnusedWarnings { implicitScopeFlagsType =>
        import implicitScopeFlagsType.Underlying as ImplicitScopeFlags
        deriveTotalTransformer[
          From,
          To,
          runtime.TransformerOverrides.Empty,
          runtime.TransformerFlags.Default,
          ImplicitScopeFlags
        ](ChimneyExpr.RuntimeDataStore.empty)
      }
    )
  )

  def deriveTotalTransformerWithConfig[
      From: WeakTypeTag,
      To: WeakTypeTag,
      Tail <: runtime.TransformerOverrides: WeakTypeTag,
      InstanceFlags <: runtime.TransformerFlags: WeakTypeTag,
      ImplicitScopeFlags <: runtime.TransformerFlags: WeakTypeTag
  ](tc: Expr[io.scalaland.chimney.dsl.TransformerConfiguration[ImplicitScopeFlags]]): Expr[Transformer[From, To]] =
    retypecheck(
      suppressWarnings(
        Expr.block(
          List(Expr.suppressUnused(tc)),
          deriveTotalTransformer[From, To, Tail, InstanceFlags, ImplicitScopeFlags](
            // Called by TransformerDefinition => prefix is TransformerDefinition
            c.Expr[dsl.TransformerDefinitionCommons.RuntimeDataStore](q"${c.prefix.tree}.runtimeData")
          )
        )
      )
    )

  def derivePartialTransformationWithConfigNoFailFast[
      From: WeakTypeTag,
      To: WeakTypeTag,
      Tail <: runtime.TransformerOverrides: WeakTypeTag,
      InstanceFlags <: runtime.TransformerFlags: WeakTypeTag,
      ImplicitScopeFlags <: runtime.TransformerFlags: WeakTypeTag
  ](tc: Expr[io.scalaland.chimney.dsl.TransformerConfiguration[ImplicitScopeFlags]]): Expr[partial.Result[To]] =
    retypecheck(
      suppressWarnings(
        // Called by PartialTransformerInto => prefix is PartialTransformerInto
        // We're caching it because it is used twice: once for RuntimeDataStore and once for source
        cacheDefinition(c.Expr[dsl.PartialTransformerInto[From, To, Tail, InstanceFlags]](c.prefix.tree)) { pti =>
          Expr.block(
            List(Expr.suppressUnused(tc)),
            derivePartialTransformationResult[From, To, Tail, InstanceFlags, ImplicitScopeFlags](
              src = c.Expr[From](q"$pti.source"),
              failFast = c.Expr[Boolean](q"false"),
              runtimeDataStore = c.Expr[dsl.TransformerDefinitionCommons.RuntimeDataStore](q"$pti.td.runtimeData")
            )
          )
        }
      )
    )

  def derivePartialTransformationWithConfigFailFast[
      From: WeakTypeTag,
      To: WeakTypeTag,
      Tail <: runtime.TransformerOverrides: WeakTypeTag,
      InstanceFlags <: runtime.TransformerFlags: WeakTypeTag,
      ImplicitScopeFlags <: runtime.TransformerFlags: WeakTypeTag
  ](tc: Expr[io.scalaland.chimney.dsl.TransformerConfiguration[ImplicitScopeFlags]]): Expr[partial.Result[To]] =
    retypecheck(
      suppressWarnings(
        // Called by PartialTransformerInto => prefix is PartialTransformerInto
        // We're caching it because it is used twice: once for RuntimeDataStore and once for source
        cacheDefinition(c.Expr[dsl.PartialTransformerInto[From, To, Tail, InstanceFlags]](c.prefix.tree)) { pti =>
          Expr.block(
            List(Expr.suppressUnused(tc)),
            derivePartialTransformationResult[From, To, Tail, InstanceFlags, ImplicitScopeFlags](
              src = c.Expr[From](q"$pti.source"),
              failFast = c.Expr[Boolean](q"true"),
              runtimeDataStore = c.Expr[dsl.TransformerDefinitionCommons.RuntimeDataStore](q"$pti.td.runtimeData")
            )
          )
        }
      )
    )

  def derivePartialTransformerWithDefaults[
      From: WeakTypeTag,
      To: WeakTypeTag
  ]: c.universe.Expr[PartialTransformer[From, To]] =
    retypecheck(
      suppressWarnings(
        resolveImplicitScopeConfigAndMuteUnusedWarnings { implicitScopeFlagsType =>
          import implicitScopeFlagsType.Underlying
          derivePartialTransformer[
            From,
            To,
            runtime.TransformerOverrides.Empty,
            runtime.TransformerFlags.Default,
            implicitScopeFlagsType.Underlying
          ](ChimneyExpr.RuntimeDataStore.empty)
        }
      )
    )

  def derivePartialTransformerWithConfig[
      From: WeakTypeTag,
      To: WeakTypeTag,
      Tail <: runtime.TransformerOverrides: WeakTypeTag,
      InstanceFlags <: runtime.TransformerFlags: WeakTypeTag,
      ImplicitScopeFlags <: runtime.TransformerFlags: WeakTypeTag
  ](
      tc: Expr[io.scalaland.chimney.dsl.TransformerConfiguration[ImplicitScopeFlags]]
  ): Expr[PartialTransformer[From, To]] = retypecheck(
    suppressWarnings(
      Expr.block(
        List(Expr.suppressUnused(tc)),
        derivePartialTransformer[From, To, Tail, InstanceFlags, ImplicitScopeFlags](
          // Called by PartialTransformerDefinition => prefix is PartialTransformerDefinition
          c.Expr[dsl.TransformerDefinitionCommons.RuntimeDataStore](q"${c.prefix.tree}.runtimeData")
        )
      )
    )
  )

  private def resolveImplicitScopeConfigAndMuteUnusedWarnings[A: Type](
      useImplicitScopeFlags: ?<[runtime.TransformerFlags] => Expr[A]
  ): Expr[A] = {
    val implicitScopeConfig = {
      import c.universe.*

      val searchTypeTree =
        tq"${typeOf[io.scalaland.chimney.dsl.TransformerConfiguration[? <: runtime.TransformerFlags]]}"
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
          reportError("Can't locate implicit TransformerConfiguration!")
          // $COVERAGE-ON$
        }
    }
    val implicitScopeFlagsType = Type.platformSpecific
      .fromUntyped[runtime.TransformerFlags](implicitScopeConfig.tpe.typeArgs.head)
      .as_?<[runtime.TransformerFlags]

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

  private def suppressWarnings[A: Type](expr: c.Expr[A]): c.Expr[A] = {
    // Scala 3 generate prefix$macro$[n] while Scala 2 prefix[n] and we want to align the behavior
    val result = c.internal.reificationSupport.freshTermName("result$macro$")
    c.Expr[A](
      q"""{
        @_root_.java.lang.SuppressWarnings(_root_.scala.Array("org.wartremover.warts.All", "all"))
        val $result = $expr
        $result
      }"""
    )
  }
}
