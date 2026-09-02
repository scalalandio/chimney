package io.scalaland.chimney.internal.compiletime.derivation.transformer

import io.scalaland.chimney.dsl
import io.scalaland.chimney.{PartialTransformer, Transformer}
import io.scalaland.chimney.internal.compiletime.PlatformBridge
import io.scalaland.chimney.internal.compiletime.derivation.RuntimeDataStoreFlattening
import io.scalaland.chimney.internal.runtime
import io.scalaland.chimney.partial

import scala.reflect.macros.blackbox

/** The constructor parameter is deliberately NOT named `c`: [[PlatformBridge]] declares `val c`, and body references
  * must resolve to the inherited member, not the constructor parameter.
  *
  * Plain quasiquotes are used instead of `Expr.quote` - its generated quasiquotes would be ambiguous with this file's
  * own `c.universe` import.
  */
final class TransformerMacros(ctx: blackbox.Context) extends PlatformBridge(ctx) with Derivation with Gateway {

  import c.universe.{internal as _, Transformer as _, *}

  def deriveTotalTransformationWithConfig[
      From: WeakTypeTag,
      To: WeakTypeTag,
      Overrides <: runtime.TransformerOverrides: WeakTypeTag,
      InstanceFlags <: runtime.TransformerFlags: WeakTypeTag,
      ImplicitScopeFlags <: runtime.TransformerFlags: WeakTypeTag
  ](
      tc: Expr[io.scalaland.chimney.dsl.TransformerConfiguration[ImplicitScopeFlags]]
  ): Expr[To] = retypecheck(
    // Called by TransformerInto => prefix is TransformerInto
    flattenedTransformationResult[From, To, Overrides, InstanceFlags, ImplicitScopeFlags](tc)(
      deriveTotalTransformationResult[From, To, Overrides, InstanceFlags, ImplicitScopeFlags](_, _)
    ).getOrElse(
      cacheDefinition(c.Expr[dsl.TransformerInto[From, To, Overrides, InstanceFlags]](c.prefix.tree)) { ti =>
        val body = deriveTotalTransformationResult[From, To, Overrides, InstanceFlags, ImplicitScopeFlags](
          src = c.Expr[From](q"$ti.source"),
          runtimeDataStore = c.Expr[dsl.TransformerDefinitionCommons.RuntimeDataStore](q"$ti.td.runtimeData")
        )
        c.Expr[To](q"{ ${Expr.suppressUnused(tc)}; $body }")
      }
    )
  )

  def deriveTotalTransformerWithDefaults[
      From: WeakTypeTag,
      To: WeakTypeTag
  ]: Expr[Transformer[From, To]] = retypecheck(
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

  def deriveTotalTransformerWithConfig[
      From: WeakTypeTag,
      To: WeakTypeTag,
      Overrides <: runtime.TransformerOverrides: WeakTypeTag,
      InstanceFlags <: runtime.TransformerFlags: WeakTypeTag,
      ImplicitScopeFlags <: runtime.TransformerFlags: WeakTypeTag
  ](
      tc: Expr[io.scalaland.chimney.dsl.TransformerConfiguration[ImplicitScopeFlags]]
  ): Expr[Transformer[From, To]] = retypecheck {
    // Called by TransformerDefinition => prefix is TransformerDefinition
    val rds = flattenedDefinitionRuntimeDataStore()
      .getOrElse(c.Expr[dsl.TransformerDefinitionCommons.RuntimeDataStore](q"${c.prefix.tree}.runtimeData"))
    val body = deriveTotalTransformer[From, To, Overrides, InstanceFlags, ImplicitScopeFlags](rds)
    c.Expr[Transformer[From, To]](q"{ ${Expr.suppressUnused(tc)}; $body }")
  }

  def derivePartialTransformationWithConfigNoFailFast[
      From: WeakTypeTag,
      To: WeakTypeTag,
      Overrides <: runtime.TransformerOverrides: WeakTypeTag,
      InstanceFlags <: runtime.TransformerFlags: WeakTypeTag,
      ImplicitScopeFlags <: runtime.TransformerFlags: WeakTypeTag
  ](
      tc: Expr[io.scalaland.chimney.dsl.TransformerConfiguration[ImplicitScopeFlags]]
  ): Expr[partial.Result[To]] = retypecheck(
    // Called by PartialTransformerInto => prefix is PartialTransformerInto
    flattenedPartialTransformationResult[From, To, Overrides, InstanceFlags, ImplicitScopeFlags](
      tc,
      failFast = c.Expr[Boolean](q"false")
    )(
      derivePartialTransformationResult[From, To, Overrides, InstanceFlags, ImplicitScopeFlags](_, _, _)
    ).getOrElse(
      cacheDefinition(c.Expr[dsl.PartialTransformerInto[From, To, Overrides, InstanceFlags]](c.prefix.tree)) { pti =>
        val body = derivePartialTransformationResult[From, To, Overrides, InstanceFlags, ImplicitScopeFlags](
          src = c.Expr[From](q"$pti.source"),
          failFast = c.Expr[Boolean](q"false"),
          runtimeDataStore = c.Expr[dsl.TransformerDefinitionCommons.RuntimeDataStore](q"$pti.td.runtimeData")
        )
        c.Expr[partial.Result[To]](q"{ ${Expr.suppressUnused(tc)}; $body }")
      }
    )
  )

  def derivePartialTransformationWithConfigFailFast[
      From: WeakTypeTag,
      To: WeakTypeTag,
      Overrides <: runtime.TransformerOverrides: WeakTypeTag,
      InstanceFlags <: runtime.TransformerFlags: WeakTypeTag,
      ImplicitScopeFlags <: runtime.TransformerFlags: WeakTypeTag
  ](
      tc: Expr[io.scalaland.chimney.dsl.TransformerConfiguration[ImplicitScopeFlags]]
  ): Expr[partial.Result[To]] = retypecheck(
    // Called by PartialTransformerInto => prefix is PartialTransformerInto
    flattenedPartialTransformationResult[From, To, Overrides, InstanceFlags, ImplicitScopeFlags](
      tc,
      failFast = c.Expr[Boolean](q"true")
    )(
      derivePartialTransformationResult[From, To, Overrides, InstanceFlags, ImplicitScopeFlags](_, _, _)
    ).getOrElse(
      cacheDefinition(c.Expr[dsl.PartialTransformerInto[From, To, Overrides, InstanceFlags]](c.prefix.tree)) { pti =>
        val body = derivePartialTransformationResult[From, To, Overrides, InstanceFlags, ImplicitScopeFlags](
          src = c.Expr[From](q"$pti.source"),
          failFast = c.Expr[Boolean](q"true"),
          runtimeDataStore = c.Expr[dsl.TransformerDefinitionCommons.RuntimeDataStore](q"$pti.td.runtimeData")
        )
        c.Expr[partial.Result[To]](q"{ ${Expr.suppressUnused(tc)}; $body }")
      }
    )
  )

  def derivePartialTransformerWithDefaults[
      From: WeakTypeTag,
      To: WeakTypeTag
  ]: c.universe.Expr[PartialTransformer[From, To]] = retypecheck(
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

  def derivePartialTransformerWithConfig[
      From: WeakTypeTag,
      To: WeakTypeTag,
      Overrides <: runtime.TransformerOverrides: WeakTypeTag,
      InstanceFlags <: runtime.TransformerFlags: WeakTypeTag,
      ImplicitScopeFlags <: runtime.TransformerFlags: WeakTypeTag
  ](
      tc: Expr[io.scalaland.chimney.dsl.TransformerConfiguration[ImplicitScopeFlags]]
  ): Expr[PartialTransformer[From, To]] = retypecheck {
    // Called by PartialTransformerDefinition => prefix is PartialTransformerDefinition
    val rds = flattenedDefinitionRuntimeDataStore()
      .getOrElse(c.Expr[dsl.TransformerDefinitionCommons.RuntimeDataStore](q"${c.prefix.tree}.runtimeData"))
    val body = derivePartialTransformer[From, To, Overrides, InstanceFlags, ImplicitScopeFlags](rds)
    c.Expr[PartialTransformer[From, To]](q"{ ${Expr.suppressUnused(tc)}; $body }")
  }

  private def resolveImplicitScopeConfigAndMuteUnusedWarnings[A: Type](
      useImplicitScopeFlags: ??<:[runtime.TransformerFlags] => Expr[A]
  ): Expr[A] = {
    val implicitScopeConfig = {
      val transformerConfigurationType =
        c.WeakTypeTag[io.scalaland.chimney.dsl.TransformerConfiguration[? <: runtime.TransformerFlags]](
          c.typecheck(
            tree = tq"${typeOf[io.scalaland.chimney.dsl.TransformerConfiguration[? <: runtime.TransformerFlags]]}",
            silent = true,
            mode = c.TYPEmode,
            withImplicitViewsDisabled = true,
            withMacrosDisabled = false
          ).tpe
        )

      Expr.summonImplicit(transformerConfigurationType).toOption.getOrElse {
        // $COVERAGE-OFF$should never happen unless someone mess around with type-level representation
        reportError("Can't locate implicit TransformerConfiguration!")
        // $COVERAGE-ON$
      }
    }
    val implicitScopeFlagsType = c
      .WeakTypeTag[runtime.TransformerFlags](implicitScopeConfig.tpe.tpe.typeArgs.head)
      .as_??<:[runtime.TransformerFlags]

    val body = useImplicitScopeFlags(implicitScopeFlagsType)
    c.Expr[A](q"{ ${Expr.suppressUnused(implicitScopeConfig)}; $body }")
  }

  private def flattenedDefinitionRuntimeDataStore(): Option[Expr[dsl.TransformerDefinitionCommons.RuntimeDataStore]] =
    RuntimeDataStoreFlattening.flattenedRuntimeDataStore(c.prefix.tree)(c)

  private def flattenedTransformationResult[
      From: WeakTypeTag,
      To: WeakTypeTag,
      Overrides <: runtime.TransformerOverrides: WeakTypeTag,
      InstanceFlags <: runtime.TransformerFlags: WeakTypeTag,
      ImplicitScopeFlags <: runtime.TransformerFlags: WeakTypeTag
  ](tc: Expr[io.scalaland.chimney.dsl.TransformerConfiguration[ImplicitScopeFlags]])(
      derive: (Expr[From], Expr[dsl.TransformerDefinitionCommons.RuntimeDataStore]) => Expr[To]
  ): Option[Expr[To]] =
    for {
      rds <- RuntimeDataStoreFlattening.flattenedRuntimeDataStore(c.prefix.tree)(c)
      args <- RuntimeDataStoreFlattening.extractBaseConstructorArgs(c.prefix.tree)(c)
      sourceTree <- args.headOption
    } yield {
      val body = derive(c.Expr[From](sourceTree), rds)
      c.Expr[To](q"{ ${Expr.suppressUnused(tc)}; $body }")
    }

  private def flattenedPartialTransformationResult[
      From: WeakTypeTag,
      To: WeakTypeTag,
      Overrides <: runtime.TransformerOverrides: WeakTypeTag,
      InstanceFlags <: runtime.TransformerFlags: WeakTypeTag,
      ImplicitScopeFlags <: runtime.TransformerFlags: WeakTypeTag
  ](tc: Expr[io.scalaland.chimney.dsl.TransformerConfiguration[ImplicitScopeFlags]], failFast: Expr[Boolean])(
      derive: (Expr[From], Expr[Boolean], Expr[dsl.TransformerDefinitionCommons.RuntimeDataStore]) => Expr[
        partial.Result[To]
      ]
  ): Option[Expr[partial.Result[To]]] =
    for {
      rds <- RuntimeDataStoreFlattening.flattenedRuntimeDataStore(c.prefix.tree)(c)
      args <- RuntimeDataStoreFlattening.extractBaseConstructorArgs(c.prefix.tree)(c)
      sourceTree <- args.headOption
    } yield {
      val body = derive(c.Expr[From](sourceTree), failFast, rds)
      c.Expr[partial.Result[To]](q"{ ${Expr.suppressUnused(tc)}; $body }")
    }

  private def retypecheck[A: Type](expr: c.Expr[A]): c.Expr[A] = try {
    val res = c.typecheck(tree = c.untypecheck(expr.tree))
    // expr->untypecheck->typecheck can loose precision, e.g. upcast literal-based singleton type to its parent
    c.Expr[A](if (res.tpe <:< Type[A].tpe) res else q"$res.asInstanceOf[${Type[A]}]")
  } catch {
    case scala.reflect.macros.TypecheckException(_, msg) => c.abort(c.enclosingPosition, msg)
  }
}
