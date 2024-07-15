package io.scalaland.chimney.internal.compiletime.derivation.iso

import io.scalaland.chimney.dsl.IsoDefinition
import io.scalaland.chimney.internal.compiletime.derivation.transformer.{DerivationPlatform, Gateway}
import io.scalaland.chimney.Iso
import io.scalaland.chimney.internal.runtime

import scala.quoted.{Expr, Quotes, Type}

final class IsoMacros(q: Quotes) extends DerivationPlatform(q) with Gateway {

  import quotes.*, quotes.reflect.*

  def deriveIsoWithDefaults[
      First: Type,
      Second: Type
  ]: Expr[Iso[First, Second]] = suppressWarnings {
    resolveImplicitScopeConfigAndMuteUnusedWarnings { implicitScopeFlagsType =>
      import implicitScopeFlagsType.Underlying as ImplicitScopeFlags
      '{
        Iso[First, Second](
          first = ${
            deriveTotalTransformer[
              First,
              Second,
              runtime.TransformerOverrides.Empty,
              runtime.TransformerFlags.Default,
              ImplicitScopeFlags
            ](runtimeDataStore = ChimneyExpr.RuntimeDataStore.empty)
          },
          second = ${
            deriveTotalTransformer[
              Second,
              First,
              runtime.TransformerOverrides.Empty,
              runtime.TransformerFlags.Default,
              ImplicitScopeFlags
            ](runtimeDataStore = ChimneyExpr.RuntimeDataStore.empty)
          }
        )
      }
    }
  }

  def deriveIsoWithConfig[
      First: Type,
      Second: Type,
      FirstOverrides <: runtime.TransformerOverrides: Type,
      SecondOverrides <: runtime.TransformerOverrides: Type,
      Flags <: runtime.TransformerFlags: Type,
      ImplicitScopeFlags <: runtime.TransformerFlags: Type
  ](
      id: Expr[IsoDefinition[First, Second, FirstOverrides, SecondOverrides, Flags]]
  ): Expr[Iso[First, Second]] = suppressWarnings {
    '{
      Iso[First, Second](
        first = ${
          deriveTotalTransformer[First, Second, FirstOverrides, Flags, ImplicitScopeFlags](runtimeDataStore = '{
            ${ id }.first.runtimeData
          })
        },
        second = ${
          deriveTotalTransformer[Second, First, SecondOverrides, Flags, ImplicitScopeFlags](runtimeDataStore = '{
            ${ id }.second.runtimeData
          })
        }
      )
    }
  }

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

object IsoMacros {

  final def deriveIsoWithDefaults[
      First: Type,
      Second: Type
  ](using quotes: Quotes): Expr[Iso[First, Second]] =
    new IsoMacros(quotes).deriveIsoWithDefaults[First, Second]

  final def deriveIsoWithConfig[
      First: Type,
      Second: Type,
      FirstOverrides <: runtime.TransformerOverrides: Type,
      SecondOverrides <: runtime.TransformerOverrides: Type,
      Flags <: runtime.TransformerFlags: Type,
      ImplicitScopeFlags <: runtime.TransformerFlags: Type
  ](
      id: Expr[IsoDefinition[First, Second, FirstOverrides, SecondOverrides, Flags]]
  )(using quotes: Quotes): Expr[Iso[First, Second]] =
    new IsoMacros(quotes)
      .deriveIsoWithConfig[First, Second, FirstOverrides, SecondOverrides, Flags, ImplicitScopeFlags](id)
}
