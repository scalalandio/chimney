package io.scalaland.chimney.internal.compiletime.derivation

import io.scalaland.chimney.internal
import io.scalaland.chimney.internal.compiletime.DefinitionsPlatform

private[compiletime] trait ConfigurationsPlatform extends Configurations { this: DefinitionsPlatform =>

  import quotes.*, quotes.reflect.*

  protected object Configurations extends ConfigurationsModule {

    protected def extractTransformerConfig[Cfg <: internal.TransformerCfg: Type](
        runtimeDataIdx: Int
    ): TransformerConfig = {
      val cfgTpe = TypeRepr.of[Cfg].dealias

      cfgTpe.asType match {
        case '[internal.TransformerCfg.Empty] =>
          TransformerConfig()
        case '[internal.TransformerCfg.FieldConst[fieldNameT, cfgTailT]] =>
          extractTransformerConfig[cfgTailT](1 + runtimeDataIdx)
            .addFieldOverride(Type[fieldNameT].asStringSingletonType, RuntimeFieldOverride.Const(runtimeDataIdx))
        case '[internal.TransformerCfg.FieldComputed[fieldNameT, cfgTailT]] =>
          extractTransformerConfig[cfgTailT](1 + runtimeDataIdx)
            .addFieldOverride(Type[fieldNameT].asStringSingletonType, RuntimeFieldOverride.Computed(runtimeDataIdx))
        case '[internal.TransformerCfg.FieldConstPartial[fieldNameT, cfgTailT]] =>
          extractTransformerConfig[cfgTailT](1 + runtimeDataIdx)
            .addFieldOverride(Type[fieldNameT].asStringSingletonType, RuntimeFieldOverride.ConstPartial(runtimeDataIdx))
        case '[internal.TransformerCfg.FieldComputedPartial[fieldNameT, cfgTailT]] =>
          extractTransformerConfig[cfgTailT](1 + runtimeDataIdx)
            .addFieldOverride(
              Type[fieldNameT].asStringSingletonType,
              RuntimeFieldOverride.ComputedPartial(runtimeDataIdx)
            )
        case '[internal.TransformerCfg.FieldRelabelled[fieldNameFromT, fieldNameToT, cfgTailT]] =>
          extractTransformerConfig[cfgTailT](1 + runtimeDataIdx)
            .addFieldOverride(
              Type[fieldNameToT].asStringSingletonType,
              RuntimeFieldOverride.RenamedFrom(Type[fieldNameFromT].asStringSingletonType)
            )
        case '[internal.TransformerCfg.CoproductInstance[instanceT, targetT, cfgTailT]] =>
          extractTransformerConfig[cfgTailT](1 + runtimeDataIdx)
            .addCoproductInstance(
              Type[instanceT].asExistential,
              Type[targetT].asExistential,
              RuntimeCoproductOverride.CoproductInstance(runtimeDataIdx)
            )
        case '[internal.TransformerCfg.CoproductInstancePartial[instanceT, targetT, cfgTailT]] =>
          extractTransformerConfig[cfgTailT](1 + runtimeDataIdx)
            .addCoproductInstance(
              Type[instanceT].asExistential,
              Type[targetT].asExistential,
              RuntimeCoproductOverride.CoproductInstancePartial(runtimeDataIdx)
            )
        case _ =>
          reportError("Bad internal transformer config type shape!")
      }
    }
  }

  extension [T <: String](tpe: Type[T]) {

    private def asStringSingletonType: String = quoted.Type.valueOfConstant[T](using tpe)(using quotes) match {
      case Some(str) => str
      case None      => assertionFailed(s"Invalid string literal type: ${tpe}")
    }
  }
}
