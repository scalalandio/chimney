package io.scalaland.chimney.internal.compiletime.derivation.transformer

import io.scalaland.chimney.dsl.{PartialTransformerDefinition, TransformerDefinition}
import io.scalaland.chimney.{PartialTransformer, Transformer}
import io.scalaland.chimney.internal.runtime
import io.scalaland.chimney.partial

import scala.quoted.{Expr, Quotes, Type}

final class TransformerMacros(q: Quotes) extends DerivationPlatform(q) with Gateway {

  import quotes.*, quotes.reflect.*

  def deriveTotalTransformerWithConfig[
      From: Type,
      To: Type,
      Overrides <: runtime.TransformerOverrides: Type,
      Flags <: runtime.TransformerFlags: Type,
      ImplicitScopeFlags <: runtime.TransformerFlags: Type
  ](
      td: Expr[TransformerDefinition[From, To, Overrides, Flags]]
  ): Expr[Transformer[From, To]] =
    deriveTotalTransformer[From, To, Overrides, Flags, ImplicitScopeFlags](runtimeDataStore = '{ $td.runtimeData })

  def deriveTotalTransformerWithDefaults[
      From: Type,
      To: Type
  ]: Expr[Transformer[From, To]] = resolveImplicitScopeConfigAndMuteUnusedWarnings { implicitScopeFlagsType =>
    import implicitScopeFlagsType.Underlying as ImplicitScopeFlags
    deriveTotalTransformer[
      From,
      To,
      runtime.TransformerOverrides.Empty,
      runtime.TransformerFlags.Default,
      ImplicitScopeFlags
    ](runtimeDataStore = ChimneyExpr.RuntimeDataStore.empty)
  }

  def derivePartialTransformerWithDefaults[
      From: Type,
      To: Type
  ]: Expr[PartialTransformer[From, To]] = resolveImplicitScopeConfigAndMuteUnusedWarnings { implicitScopeFlagsType =>
    import implicitScopeFlagsType.Underlying as ImplicitScopeFlags
    derivePartialTransformer[
      From,
      To,
      runtime.TransformerOverrides.Empty,
      runtime.TransformerFlags.Default,
      ImplicitScopeFlags
    ](runtimeDataStore = ChimneyExpr.RuntimeDataStore.empty)
  }

  def derivePartialTransformerWithConfig[
      From: Type,
      To: Type,
      Overrides <: runtime.TransformerOverrides: Type,
      Flags <: runtime.TransformerFlags: Type,
      ImplicitScopeFlags <: runtime.TransformerFlags: Type
  ](
      td: Expr[PartialTransformerDefinition[From, To, Overrides, Flags]]
  ): Expr[PartialTransformer[From, To]] =
    derivePartialTransformer[From, To, Overrides, Flags, ImplicitScopeFlags](runtimeDataStore = '{
      $td.runtimeData
    })

  private def resolveImplicitScopeConfigAndMuteUnusedWarnings[A: Type](
      useImplicitScopeFlags: ?<[runtime.TransformerFlags] => Expr[A]
  ): Expr[A] = {
    val implicitScopeConfig = scala.quoted.Expr
      .summon[io.scalaland.chimney.dsl.TransformerConfiguration[? <: runtime.TransformerFlags]]
      .getOrElse {
        // $COVERAGE-OFF$should never happen unless someone mess around with type-level representation
        reportError("Can't locate implicit TransformerConfiguration!")
        // $COVERAGE-ON$
      }
    val implicitScopeFlagsType = implicitScopeConfig.asTerm.tpe.widen.typeArgs.head.asType
      .asInstanceOf[Type[runtime.TransformerFlags]]
      .as_?<[runtime.TransformerFlags]

    Expr.block(
      List(Expr.suppressUnused(implicitScopeConfig)),
      useImplicitScopeFlags(implicitScopeFlagsType)
    )
  }
}

object TransformerMacros {

  final def deriveTotalTransformerWithDefaults[
      From: Type,
      To: Type
  ](using quotes: Quotes): Expr[Transformer[From, To]] =
    new TransformerMacros(quotes).deriveTotalTransformerWithDefaults[From, To]

  final def deriveTotalTransformerWithConfig[
      From: Type,
      To: Type,
      Overrides <: runtime.TransformerOverrides: Type,
      Flags <: runtime.TransformerFlags: Type,
      ImplicitScopeFlags <: runtime.TransformerFlags: Type
  ](
      td: Expr[TransformerDefinition[From, To, Overrides, Flags]]
  )(using quotes: Quotes): Expr[Transformer[From, To]] = {
    val (overridesType, flagsType) = resolveOverridesAndFlagsTypes(td)
    (overridesType, flagsType) match {
      case ('[o], '[f]) =>
        new TransformerMacros(quotes)
          .deriveTotalTransformerWithConfig[From, To, o & runtime.TransformerOverrides, f & runtime.TransformerFlags, ImplicitScopeFlags](
            td.asInstanceOf[Expr[TransformerDefinition[From, To, o & runtime.TransformerOverrides, f & runtime.TransformerFlags]]]
          )
    }
  }

  final def deriveTotalTransformerResultWithConfig[
      From: Type,
      To: Type,
      Overrides <: runtime.TransformerOverrides: Type,
      Flags <: runtime.TransformerFlags: Type,
      ImplicitScopeFlags <: runtime.TransformerFlags: Type
  ](source: Expr[From], td: Expr[TransformerDefinition[From, To, Overrides, Flags]])(using quotes: Quotes): Expr[To] = {
    val (overridesType, flagsType) = resolveOverridesAndFlagsTypes(td)
    (overridesType, flagsType) match {
      case ('[o], '[f]) =>
        val typedTd = td.asInstanceOf[Expr[TransformerDefinition[From, To, o & runtime.TransformerOverrides, f & runtime.TransformerFlags]]]
        new TransformerMacros(quotes).deriveTotalTransformationResult[From, To, o & runtime.TransformerOverrides, f & runtime.TransformerFlags, ImplicitScopeFlags](
          source,
          '{ $typedTd.runtimeData }
        )
    }
  }

  final def derivePartialTransformerWithDefaults[
      From: Type,
      To: Type
  ](using quotes: Quotes): Expr[PartialTransformer[From, To]] =
    new TransformerMacros(quotes).derivePartialTransformerWithDefaults[From, To]

  final def derivePartialTransformerWithConfig[
      From: Type,
      To: Type,
      Overrides <: runtime.TransformerOverrides: Type,
      Flags <: runtime.TransformerFlags: Type,
      ImplicitScopeFlags <: runtime.TransformerFlags: Type
  ](
      td: Expr[PartialTransformerDefinition[From, To, Overrides, Flags]]
  )(using quotes: Quotes): Expr[PartialTransformer[From, To]] = {
    val (overridesType, flagsType) = resolveOverridesAndFlagsTypes(td)
    (overridesType, flagsType) match {
      case ('[o], '[f]) =>
        new TransformerMacros(quotes)
          .derivePartialTransformerWithConfig[From, To, o & runtime.TransformerOverrides, f & runtime.TransformerFlags, ImplicitScopeFlags](
            td.asInstanceOf[Expr[PartialTransformerDefinition[From, To, o & runtime.TransformerOverrides, f & runtime.TransformerFlags]]]
          )
    }
  }

  final def derivePartialTransformerResultWithConfig[
      From: Type,
      To: Type,
      Overrides <: runtime.TransformerOverrides: Type,
      Flags <: runtime.TransformerFlags: Type,
      ImplicitScopeFlags <: runtime.TransformerFlags: Type
  ](source: Expr[From], td: Expr[PartialTransformerDefinition[From, To, Overrides, Flags]], failFast: Boolean)(using
      quotes: Quotes
  ): Expr[partial.Result[To]] = {
    val (overridesType, flagsType) = resolveOverridesAndFlagsTypes(td)
    (overridesType, flagsType) match {
      case ('[o], '[f]) =>
        val typedTd = td.asInstanceOf[Expr[PartialTransformerDefinition[From, To, o & runtime.TransformerOverrides, f & runtime.TransformerFlags]]]
        new TransformerMacros(quotes).derivePartialTransformationResult[From, To, o & runtime.TransformerOverrides, f & runtime.TransformerFlags, ImplicitScopeFlags](
          source,
          Expr(failFast),
          '{ $typedTd.runtimeData }
        )
    }
  }

  // When inline def methods are chained (e.g. .withFieldConst(...).enableDefaultValues.buildTransformer)
  // inside another inline def, the Scala 3 compiler may represent Overrides/Flags as path-dependent types
  // (e.g. TransformerDefinition_this.Overrides) rather than resolving them to their concrete forms.
  // We resolve these by extracting actual type arguments from the 'this' expression's widened type. (see #857)
  private def resolveOverridesAndFlagsTypes(td: Expr[?])(using quotes: Quotes): (Type[?], Type[?]) = {
    import quotes.reflect.*

    def extractAllArgs(tpe: TypeRepr): Option[(Type[?], Type[?])] = {
      val args = tpe.dealias.typeArgs
      if (args.length >= 4) {
        // Both Overrides (index 2) and Flags (index 3) must be concrete (not TypeBounds/wildcards)
        (args(2), args(3)) match {
          case (_: TypeBounds, _) | (_, _: TypeBounds) => None
          case _                                       => Some((args(2).asType, args(3).asType))
        }
      } else None
    }

    val term = td.asTerm
    val widened = term.tpe.widen

    // Try direct type args first (common case when types are concrete)
    extractAllArgs(widened)
      .orElse {
        // For inline def reuse (#857), the widened type may have wildcards for Overrides
        // (e.g. TransformerDefinition[..., ? <: TransformerOverrides, ...].UpdateFlag[...]).
        // Strategy: extract Flags from the base type (it's concrete even when Overrides isn't),
        // and extract Overrides by inspecting the val definition's RHS tree.
        val tdClassSym = TypeRepr.of[TransformerDefinition[?, ?, ?, ?]].typeSymbol
        val ptdClassSym = TypeRepr.of[PartialTransformerDefinition[?, ?, ?, ?]].typeSymbol

        // Get Flags from base type (args(3) is usually concrete)
        val flagsFromBaseType: Option[Type[?]] = widened.baseClasses.iterator
          .filter(cls => cls == tdClassSym || cls == ptdClassSym)
          .map(cls => widened.baseType(cls).typeArgs)
          .collectFirst { case args if args.length >= 4 && !args(3).match { case TypeBounds(_, _) => true; case _ => false } =>
            args(3).asType
          }

        // Get Overrides from the val definition's RHS tree
        val overridesFromTree: Option[Type[?]] = {
          val inner = term match { case Inlined(_, _, i) => i; case t => t }
          val sym = inner.symbol
          if (sym.isValDef) {
            sym.tree match {
              case ValDef(_, _, Some(rhs)) =>
                def findOverrides(t: Tree): Option[Type[?]] = t match {
                  case Select(qualifier, _) =>
                    val qTpe = qualifier.asInstanceOf[Term].tpe.widen.dealias
                    val args = qTpe.typeArgs
                    if (args.length >= 4 && !args(2).match { case TypeBounds(_, _) => true; case _ => false }) Some(args(2).asType)
                    else findOverrides(qualifier)
                  case Inlined(_, _, inner) => findOverrides(inner)
                  case Block(_, expr)       => findOverrides(expr)
                  case TypeApply(inner, _) =>
                    val tTpe = t.asInstanceOf[Term].tpe.widen.dealias
                    val args = tTpe.typeArgs
                    if (args.length >= 4 && !args(2).match { case TypeBounds(_, _) => true; case _ => false }) Some(args(2).asType)
                    else findOverrides(inner)
                  case _ => None
                }
                findOverrides(rhs)
              case _ => None
            }
          } else None
        }

        for {
          overrides <- overridesFromTree
          flags <- flagsFromBaseType
        } yield (overrides, flags)
      }
      .getOrElse {
        report.errorAndAbort(
          s"Could not resolve Overrides and Flags type arguments from: ${term.tpe.show}"
        )
      }
  }
}
