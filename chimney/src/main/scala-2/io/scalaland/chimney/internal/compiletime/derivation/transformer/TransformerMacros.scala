package io.scalaland.chimney.internal.compiletime.derivation.transformer

import io.scalaland.chimney.dsl.{PartialTransformerDefinition, TransformerDefinition, TransformerDefinitionCommons}
import io.scalaland.chimney.internal.TransformerCfg.Empty
import io.scalaland.chimney.internal.TransformerFlags.Default
import io.scalaland.chimney.{internal, PartialTransformer, Transformer}
import io.scalaland.chimney.partial

import scala.reflect.macros.blackbox

final class TransformerMacros(val c: blackbox.Context) extends DerivationPlatform with Gateway {

  import c.universe.{internal as _, Transformer as _, *}

  type ImplicitScopeFlagsType <: internal.TransformerFlags

  private val that = c.prefix.tree

  def deriveTotalTransformationWithConfig[
      From: c.WeakTypeTag,
      To: c.WeakTypeTag,
      Cfg <: internal.TransformerCfg: c.WeakTypeTag,
      InstanceFlags <: internal.TransformerFlags: c.WeakTypeTag,
      ImplicitScopeFlags <: internal.TransformerFlags: c.WeakTypeTag
  ](
      tc: Expr[io.scalaland.chimney.dsl.TransformerConfiguration[ImplicitScopeFlags]]
  ): Expr[To] = retypecheck(
    muteUnusedWarnings(
      tc,
      cacheTransformerDefinition(c.Expr[TransformerDefinition[From, To, Cfg, InstanceFlags]](q"$that.td")) {
        runtimeDataStore =>
          deriveTotalTransformationResult[From, To, Cfg, InstanceFlags, ImplicitScopeFlags](
            src = c.Expr[From](q"$that.source"),
            runtimeDataStore = runtimeDataStore
          )
      }
    )
  )

  def deriveTotalTransformerWithDefaults[
      From: c.WeakTypeTag,
      To: c.WeakTypeTag
  ]: Expr[Transformer[From, To]] = retypecheck(
    resolveImplicitScopeConfigAndMuteUnusedWarnings { implicit ImplicitScopeFlagsType: Type[ImplicitScopeFlagsType] =>
      deriveTotalTransformer[From, To, Empty, Default, ImplicitScopeFlagsType](ChimneyExpr.RuntimeDataStore.empty)
    }
  )

  def deriveTotalTransformerWithConfig[
      From: c.WeakTypeTag,
      To: c.WeakTypeTag,
      Cfg <: internal.TransformerCfg: c.WeakTypeTag,
      InstanceFlags <: internal.TransformerFlags: c.WeakTypeTag,
      ImplicitScopeFlags <: internal.TransformerFlags: c.WeakTypeTag
  ](tc: Expr[io.scalaland.chimney.dsl.TransformerConfiguration[ImplicitScopeFlags]]): Expr[Transformer[From, To]] =
    retypecheck(
      muteUnusedWarnings(
        tc,
        cacheTransformerDefinition(c.Expr[TransformerDefinition[From, To, Cfg, InstanceFlags]](q"$that")) {
          runtimeDataStore =>
            deriveTotalTransformer[From, To, Cfg, InstanceFlags, ImplicitScopeFlags](runtimeDataStore)
        }
      )
    )

  def derivePartialTransformationWithConfigNoFailFast[
      From: c.WeakTypeTag,
      To: c.WeakTypeTag,
      Cfg <: internal.TransformerCfg: c.WeakTypeTag,
      InstanceFlags <: internal.TransformerFlags: c.WeakTypeTag,
      ImplicitScopeFlags <: internal.TransformerFlags: c.WeakTypeTag
  ](tc: Expr[io.scalaland.chimney.dsl.TransformerConfiguration[ImplicitScopeFlags]]): Expr[partial.Result[To]] =
    retypecheck(
      muteUnusedWarnings(
        tc,
        cachePartialTransformerDefinition(
          c.Expr[PartialTransformerDefinition[From, To, Cfg, InstanceFlags]](q"$that.td")
        ) { runtimeDataStore =>
          derivePartialTransformationResult[From, To, Cfg, InstanceFlags, ImplicitScopeFlags](
            src = c.Expr[From](q"$that.source"),
            failFast = c.Expr[Boolean](q"false"),
            runtimeDataStore = runtimeDataStore
          )
        }
      )
    )

  def derivePartialTransformationWithConfigFailFast[
      From: c.WeakTypeTag,
      To: c.WeakTypeTag,
      Cfg <: internal.TransformerCfg: c.WeakTypeTag,
      InstanceFlags <: internal.TransformerFlags: c.WeakTypeTag,
      ImplicitScopeFlags <: internal.TransformerFlags: c.WeakTypeTag
  ](tc: Expr[io.scalaland.chimney.dsl.TransformerConfiguration[ImplicitScopeFlags]]): Expr[partial.Result[To]] =
    retypecheck(
      muteUnusedWarnings(
        tc,
        cachePartialTransformerDefinition(
          c.Expr[PartialTransformerDefinition[From, To, Cfg, InstanceFlags]](q"$that.td")
        ) { runtimeDataStore =>
          derivePartialTransformationResult[From, To, Cfg, InstanceFlags, ImplicitScopeFlags](
            src = c.Expr[From](q"$that.source"),
            failFast = c.Expr[Boolean](q"true"),
            runtimeDataStore = runtimeDataStore
          )
        }
      )
    )

  def derivePartialTransformerWithDefaults[
      From: c.WeakTypeTag,
      To: c.WeakTypeTag
  ]: c.universe.Expr[PartialTransformer[From, To]] =
    retypecheck(
      resolveImplicitScopeConfigAndMuteUnusedWarnings { implicit ImplicitScopeFlagsType: Type[ImplicitScopeFlagsType] =>
        derivePartialTransformer[From, To, Empty, Default, ImplicitScopeFlagsType](ChimneyExpr.RuntimeDataStore.empty)
      }
    )

  def derivePartialTransformerWithConfig[
      From: c.WeakTypeTag,
      To: c.WeakTypeTag,
      Cfg <: internal.TransformerCfg: c.WeakTypeTag,
      InstanceFlags <: internal.TransformerFlags: c.WeakTypeTag,
      ImplicitScopeFlags <: internal.TransformerFlags: c.WeakTypeTag
  ](
      tc: Expr[io.scalaland.chimney.dsl.TransformerConfiguration[ImplicitScopeFlags]]
  ): Expr[PartialTransformer[From, To]] = retypecheck(
    muteUnusedWarnings(
      tc,
      cachePartialTransformerDefinition(c.Expr[PartialTransformerDefinition[From, To, Cfg, InstanceFlags]](q"$that")) {
        runtimeDataStore =>
          derivePartialTransformer[From, To, Cfg, InstanceFlags, ImplicitScopeFlags](runtimeDataStore)
      }
    )
  )

  private def cacheTransformerDefinition[
      From: c.WeakTypeTag,
      To: c.WeakTypeTag,
      Cfg <: internal.TransformerCfg: c.WeakTypeTag,
      InstanceFlags <: internal.TransformerFlags: c.WeakTypeTag,
      Out: c.WeakTypeTag
  ](
      td: Expr[TransformerDefinition[From, To, Cfg, InstanceFlags]]
  )(use: Expr[TransformerDefinitionCommons.RuntimeDataStore] => Expr[Out]): Expr[Out] = {
    val tdn = c.internal.reificationSupport.freshTermName("td")
    c.Expr[Out](
      q"""
       val $tdn = $td
       ${muteUnusedWarnings(
          c.Expr(q"$tdn"),
          use(c.Expr[TransformerDefinitionCommons.RuntimeDataStore](q"$tdn.runtimeData"))
        )}
       """
    )
  }

  private def cachePartialTransformerDefinition[
      From: c.WeakTypeTag,
      To: c.WeakTypeTag,
      Cfg <: internal.TransformerCfg: c.WeakTypeTag,
      InstanceFlags <: internal.TransformerFlags: c.WeakTypeTag,
      Out: c.WeakTypeTag
  ](
      td: Expr[PartialTransformerDefinition[From, To, Cfg, InstanceFlags]]
  )(use: Expr[TransformerDefinitionCommons.RuntimeDataStore] => Expr[Out]): Expr[Out] = {
    val tdn = c.internal.reificationSupport.freshTermName("td")
    c.Expr[Out](
      q"""
       val $tdn = $td
       ${muteUnusedWarnings(
          c.Expr(q"$tdn"),
          use(c.Expr[TransformerDefinitionCommons.RuntimeDataStore](q"$tdn.runtimeData"))
        )}
       """
    )
  }

  private def muteUnusedWarnings[A, B](exprA: Expr[A], exprB: Expr[B]): Expr[B] = Expr.block(
    List(Expr.suppressUnused(exprA)),
    exprB
  )

  private def findImplicitScopeTransformerConfiguration: c.universe.Tree = {
    import c.universe.*

    val searchTypeTree =
      tq"${typeOf[io.scalaland.chimney.dsl.TransformerConfiguration[? <: io.scalaland.chimney.internal.TransformerFlags]]}"
    inferImplicitTpe(searchTypeTree).getOrElse {
      // $COVERAGE-OFF$
      reportError("Can't locate implicit TransformerConfiguration!")
      // $COVERAGE-ON$
    }
  }

  private def inferImplicitTpe(tpeTree: c.universe.Tree): Option[c.universe.Tree] = {
    val typedTpeTree = c.typecheck(
      tree = tpeTree,
      silent = true,
      mode = c.TYPEmode,
      withImplicitViewsDisabled = true,
      withMacrosDisabled = false
    )

    scala.util
      .Try(c.inferImplicitValue(typedTpeTree.tpe, silent = true, withMacrosDisabled = false))
      .toOption
      .filterNot(_ == c.universe.EmptyTree)
  }

  private def resolveImplicitScopeConfigAndMuteUnusedWarnings[A: Type](
      useImplicitScopeFlags: Type[ImplicitScopeFlagsType] => Expr[A]
  ): Expr[A] = {
    val implicitScopeConfig = findImplicitScopeTransformerConfiguration
    val implicitScopeConfigType: Type[ImplicitScopeFlagsType] =
      Type.platformSpecific.fromUntyped[ImplicitScopeFlagsType](implicitScopeConfig.tpe.typeArgs.head)

    muteUnusedWarnings(
      c.Expr[ImplicitScopeFlagsType](implicitScopeConfig),
      useImplicitScopeFlags(implicitScopeConfigType)
    )
  }

  private def retypecheck[A: Type](expr: c.Expr[A]): c.Expr[A] = try
    c.Expr[A](c.typecheck(tree = c.untypecheck(expr.tree.asInstanceOf[c.Tree])))
  catch {
    case scala.reflect.macros.TypecheckException(_, msg) => c.abort(c.enclosingPosition, msg)
  }
}
