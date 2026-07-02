package io.scalaland.chimney.internal.compiletime2.derivation.codec

import io.scalaland.chimney.dsl.CodecDefinition
import io.scalaland.chimney.internal.compiletime2.PlatformBridge
import io.scalaland.chimney.internal.compiletime2.derivation.transformer.{Derivation, Gateway}
import io.scalaland.chimney.Codec
import io.scalaland.chimney.internal.runtime

import scala.quoted.{Expr, Quotes, Type}

/** Hearth-based port of `...compiletime.derivation.codec.CodecMacros` (Scala 3).
  *
  * Public methods (names, signatures, type params) of both the class and the companion mirror the old ones 1:1 so that
  * the binding sites in `io.scalaland.chimney.dsl.*` can flip packages mechanically in the next phase.
  *
  * Differences vs the old version: same as
  * [[io.scalaland.chimney.internal.compiletime2.derivation.transformer.TransformerMacros]] - extends [[PlatformBridge]]
  * + the now-shared transformer `Derivation`/`Gateway` instead of the old per-platform `DerivationPlatform`,
  * `Expr.block` -> `blockExpr`, `?<`/`.as_?<` -> `??<:`/`.as_??<:`.
  */
final class CodecMacros(q: Quotes) extends PlatformBridge(q) with Derivation with Gateway {

  import quotes.*, quotes.reflect.*

  def deriveCodecWithDefaults[
      Domain: Type,
      Dto: Type
  ]: Expr[Codec[Domain, Dto]] = resolveImplicitScopeConfigAndMuteUnusedWarnings { implicitScopeFlagsType =>
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

  def deriveCodecWithConfig[
      Domain: Type,
      Dto: Type,
      EncodeOverrides <: runtime.TransformerOverrides: Type,
      DecodeOverrides <: runtime.TransformerOverrides: Type,
      Flags <: runtime.TransformerFlags: Type,
      ImplicitScopeFlags <: runtime.TransformerFlags: Type
  ](
      cd: Expr[CodecDefinition[Domain, Dto, EncodeOverrides, DecodeOverrides, Flags]]
  ): Expr[Codec[Domain, Dto]] =
    '{
      Codec[Domain, Dto](
        encode = ${
          deriveTotalTransformer[Domain, Dto, EncodeOverrides, Flags, ImplicitScopeFlags](runtimeDataStore = '{
            $cd.encode.runtimeData
          })
        },
        decode = ${
          derivePartialTransformer[Dto, Domain, DecodeOverrides, Flags, ImplicitScopeFlags](runtimeDataStore = '{
            $cd.decode.runtimeData
          })
        }
      )
    }

  private def resolveImplicitScopeConfigAndMuteUnusedWarnings[A: Type](
      useImplicitScopeFlags: ??<:[runtime.TransformerFlags] => Expr[A]
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
      .as_??<:[runtime.TransformerFlags]

    blockExpr(
      List(Expr.suppressUnused(implicitScopeConfig)),
      useImplicitScopeFlags(implicitScopeFlagsType)
    )
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
