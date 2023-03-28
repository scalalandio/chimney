package io.scalaland.chimney.internal.compiletime.derivation

import io.scalaland.chimney.internal.compiletime.{ConfigurationsPlatform, Contexts, DefinitionsPlatform}
import io.scalaland.chimney.internal.macros.dsl.TransformerBlackboxMacros
import io.scalaland.chimney.{PartialTransformer, Transformer}

private[compiletime] trait LegacyPlatform extends Legacy {
  this: DefinitionsPlatform & ConfigurationsPlatform & Contexts =>

  protected object legacy extends LegacyImpl {

    private def convertToLegacyConfig[From, To](
        tbm: TransformerBlackboxMacros
    )(implicit ctx: Context.ForTransformer[From, To]): tbm.TransformerConfig = {
      // TODO: convert our new config into our old config
      ???
    }

    override def legacyTotalTransformerDerivation[From, To](implicit
        ctx: Context.ForTotal[From, To]
    ): Expr[Transformer[From, To]] = {
      val legacy = new TransformerBlackboxMacros(c)
      legacy
        .buildDefinedTransformerFromConfig(
          convertToLegacyConfig(legacy)
        )
        .asInstanceOf[Expr[Transformer[From, To]]]
    }

    override def legacyPartialTransformerDerivation[From, To](implicit
        ctx: Context.ForPartial[From, To]
    ): Expr[PartialTransformer[From, To]] = {
      val legacy = new TransformerBlackboxMacros(c)
      legacy
        .buildDefinedTransformerFromConfig(
          convertToLegacyConfig(legacy)
        )
        .asInstanceOf[Expr[PartialTransformer[From, To]]]
    }
  }
}
