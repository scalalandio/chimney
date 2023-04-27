package io.scalaland.chimney.internal.compiletime.derivation.transformer

import io.scalaland.chimney.{internal, partial, PartialTransformer, Transformer}
import io.scalaland.chimney.internal.compiletime.DefinitionsPlatform

import scala.quoted.{Expr, Quotes, Type}

final class TransformerMacros(q: Quotes)
    extends DefinitionsPlatform(using q)
    with DerivationPlatform
    with GatewayPlatform {

  final def deriveTotalTransformerWithDefaults[
      From: Type,
      To: Type
  ](using quotes: Quotes): Expr[Transformer[From, To]] =
    resolveLocalTransformerConfigAndMuteUnusedConfigWarnings(LocalConfigType =>
      deriveTotalTransformer(using
        Type[From],
        Type[To],
        ChimneyType.TransformerCfg.Empty,
        ChimneyType.TransformerFlags.Default,
        LocalConfigType
      )
    )(using ChimneyType.Transformer[From, To])

  final def derivePartialTransformerWithDefaults[
      From: Type,
      To: Type
  ](using quotes: Quotes): Expr[PartialTransformer[From, To]] =
    resolveLocalTransformerConfigAndMuteUnusedConfigWarnings(LocalConfigType =>
      derivePartialTransformer(using
        Type[From],
        Type[To],
        ChimneyType.TransformerCfg.Empty,
        ChimneyType.TransformerFlags.Default,
        LocalConfigType
      )
    )(using ChimneyType.PartialTransformer[From, To])

  private def findLocalTransformerConfigurationFlags(using
      quotes: Quotes
  ): Expr[io.scalaland.chimney.dsl.TransformerConfiguration[? <: io.scalaland.chimney.internal.TransformerFlags]] =
    scala.quoted.Expr
      .summon[io.scalaland.chimney.dsl.TransformerConfiguration[? <: io.scalaland.chimney.internal.TransformerFlags]]
      .getOrElse {
        // $COVERAGE-OFF$
        reportError("Can't locate implicit TransformerConfiguration!")
        // $COVERAGE-ON$
      }

  private def resolveLocalTransformerConfigAndMuteUnusedConfigWarnings[A: Type](
      useLocalConfig: Type[internal.TransformerFlags] => Expr[A]
  ): Expr[A] = {
    import quotes.*
    import quotes.reflect.*

    val localConfig = findLocalTransformerConfigurationFlags
    val localConfigType = findLocalTransformerConfigurationFlags.asTerm.tpe.widen.typeArgs.head.asType
      .asInstanceOf[Type[internal.TransformerFlags]]

    '{
      val _ = $localConfig
      ${ useLocalConfig(localConfigType) }
    }
  }
}

object TransformerMacros {

  final def deriveTotalTransformerWithDefaults[
      From: Type,
      To: Type
  ](using quotes: Quotes): Expr[Transformer[From, To]] =
    new TransformerMacros(quotes).deriveTotalTransformerWithDefaults[From, To]

  final def derivePartialTransformerWithDefaults[
      From: Type,
      To: Type
  ](using quotes: Quotes): Expr[PartialTransformer[From, To]] =
    new TransformerMacros(quotes).derivePartialTransformerWithDefaults[From, To]
}
