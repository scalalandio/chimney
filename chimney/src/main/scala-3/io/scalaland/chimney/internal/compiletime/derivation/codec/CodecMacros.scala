package io.scalaland.chimney.internal.compiletime.derivation.codec

import io.scalaland.chimney.dsl.CodecDefinition
import io.scalaland.chimney.internal.compiletime.derivation.transformer.{DerivationPlatform, Gateway}
import io.scalaland.chimney.Codec
import io.scalaland.chimney.internal.runtime

import scala.quoted.{Expr, Quotes, Type}

final class CodecMacros(q: Quotes) extends DerivationPlatform(q) with Gateway {

  import quotes.*, quotes.reflect.*

  def deriveCodecWithDefaults[
      Domain: Type,
      Dto: Type
  ]: Expr[Codec[Domain, Dto]] = suppressWarnings {
    resolveImplicitScopeConfigAndMuteUnusedWarnings { implicitScopeFlagsType =>
      import implicitScopeFlagsType.Underlying as ImplicitScopeFlags
      '{
        Codec[Domain, Dto](
          encode = ${
            deriveTotalTransformer[
              Domain,
              Dto,
              runtime.TransformerOverrides.Empty,
              runtime.TransformerFlags.Default,
              ImplicitScopeFlags
            ](runtimeDataStore = ChimneyExpr.RuntimeDataStore.empty)
          },
          decode = ${
            derivePartialTransformer[
              Dto,
              Domain,
              runtime.TransformerOverrides.Empty,
              runtime.TransformerFlags.Default,
              ImplicitScopeFlags
            ](runtimeDataStore = ChimneyExpr.RuntimeDataStore.empty)
          }
        )
      }
    }
  }

  def deriveCodecWithConfig[
      Domain: Type,
      Dto: Type,
      EncodeOverrides <: runtime.TransformerOverrides: Type,
      DecodeOverrides <: runtime.TransformerOverrides: Type,
      Flags <: runtime.TransformerFlags: Type,
      ImplicitScopeFlags <: runtime.TransformerFlags: Type
  ](
      cd: Expr[CodecDefinition[Domain, Dto, EncodeOverrides, DecodeOverrides, Flags]]
  ): Expr[Codec[Domain, Dto]] = suppressWarnings {
    '{
      Codec[Domain, Dto](
        encode = ${
          deriveTotalTransformer[Domain, Dto, EncodeOverrides, Flags, ImplicitScopeFlags](runtimeDataStore = '{
            ${ cd }.encode.runtimeData
          })
        },
        decode = ${
          derivePartialTransformer[Dto, Domain, DecodeOverrides, Flags, ImplicitScopeFlags](runtimeDataStore = '{
            ${ cd }.decode.runtimeData
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

  private def suppressWarnings[A: Type](expr: Expr[A]): Expr[A] = '{
    @SuppressWarnings(Array("org.wartremover.warts.All", "all"))
    val result = ${ expr }
    result
  }
}

object CodecMacros {

  final def deriveCodecWithDefaults[
      Domain: Type,
      Dto: Type
  ](using quotes: Quotes): Expr[Codec[Domain, Dto]] =
    new CodecMacros(quotes).deriveCodecWithDefaults[Domain, Dto]

  final def deriveCodecWithConfig[
      Domain: Type,
      Dto: Type,
      EncodeOverrides <: runtime.TransformerOverrides: Type,
      DecodeOverrides <: runtime.TransformerOverrides: Type,
      Flags <: runtime.TransformerFlags: Type,
      ImplicitScopeFlags <: runtime.TransformerFlags: Type
  ](
      cd: Expr[CodecDefinition[Domain, Dto, EncodeOverrides, DecodeOverrides, Flags]]
  )(using quotes: Quotes): Expr[Codec[Domain, Dto]] =
    new CodecMacros(quotes)
      .deriveCodecWithConfig[Domain, Dto, EncodeOverrides, DecodeOverrides, Flags, ImplicitScopeFlags](cd)
}
