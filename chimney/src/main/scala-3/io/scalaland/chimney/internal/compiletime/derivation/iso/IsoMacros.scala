package io.scalaland.chimney.internal.compiletime.derivation.iso

import io.scalaland.chimney.dsl.IsoDefinition
import io.scalaland.chimney.internal.compiletime.derivation.transformer.{DerivationPlatform, Gateway}
import io.scalaland.chimney.Iso
import io.scalaland.chimney.internal.runtime

import scala.quoted.{Expr, Quotes, Type}

final class IsoMacros(q: Quotes) extends DerivationPlatform(q) with Gateway {

  import quotes.*, quotes.reflect.*

  def deriveIsoWithDefaults[
      From: Type,
      To: Type
  ]: Expr[Iso[From, To]] = suppressWarnings {
    resolveImplicitScopeConfigAndMuteUnusedWarnings { implicitScopeFlagsType =>
      import implicitScopeFlagsType.Underlying as ImplicitScopeFlags
      '{
        Iso[From, To](
          from = ${
            deriveTotalTransformer[
              From,
              To,
              runtime.TransformerOverrides.Empty,
              runtime.TransformerFlags.Default,
              ImplicitScopeFlags
            ](runtimeDataStore = ChimneyExpr.RuntimeDataStore.empty)
          },
          to = ${
            deriveTotalTransformer[
              To,
              From,
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
      From: Type,
      To: Type,
      FromOverrides <: runtime.TransformerOverrides: Type,
      ToOverrides <: runtime.TransformerOverrides: Type,
      Flags <: runtime.TransformerFlags: Type,
      ImplicitScopeFlags <: runtime.TransformerFlags: Type
  ](
      id: Expr[IsoDefinition[From, To, FromOverrides, ToOverrides, Flags]]
  ): Expr[Iso[From, To]] = suppressWarnings {
    '{
      Iso[From, To](
        from = ${
          deriveTotalTransformer[From, To, FromOverrides, Flags, ImplicitScopeFlags](runtimeDataStore = '{
            ${ id }.from.runtimeData
          })
        },
        to = ${
          deriveTotalTransformer[To, From, ToOverrides, Flags, ImplicitScopeFlags](runtimeDataStore = '{
            ${ id }.to.runtimeData
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
        // $COVERAGE-OFF$
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

  private def suppressWarnings[A: Type](expr: Expr[A]): Expr[A] = '{
    @SuppressWarnings(Array("org.wartremover.warts.All", "all"))
    val result = ${ expr }
    result
  }
}

object IsoMacros {

  final def deriveIsoWithDefaults[
      From: Type,
      To: Type
  ](using quotes: Quotes): Expr[Iso[From, To]] =
    new IsoMacros(quotes).deriveIsoWithDefaults[From, To]

  final def deriveIsoWithConfig[
      From: Type,
      To: Type,
      FromOverrides <: runtime.TransformerOverrides: Type,
      ToOverrides <: runtime.TransformerOverrides: Type,
      Flags <: runtime.TransformerFlags: Type,
      ImplicitScopeFlags <: runtime.TransformerFlags: Type
  ](
      id: Expr[IsoDefinition[From, To, FromOverrides, ToOverrides, Flags]]
  )(using quotes: Quotes): Expr[Iso[From, To]] =
    new IsoMacros(quotes).deriveIsoWithConfig[From, To, FromOverrides, ToOverrides, Flags, ImplicitScopeFlags](id)
}
