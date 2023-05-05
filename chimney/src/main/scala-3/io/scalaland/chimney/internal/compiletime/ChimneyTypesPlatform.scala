package io.scalaland.chimney.internal.compiletime

import io.scalaland.chimney.dsl as dsls
import io.scalaland.chimney.internal
import io.scalaland.chimney.{partial, PartialTransformer, Patcher, Transformer}
import io.scalaland.chimney.dsl.{ImplicitTransformerPreference, TransformerDefinitionCommons}

import scala.quoted

private[compiletime] trait ChimneyTypesPlatform extends ChimneyTypes { this: DefinitionsPlatform =>

  import quotes.*
  import quotes.reflect.*

  object ChimneyType extends ChimneyTypeModule {
    import typeUtils.*

    def Transformer[From: Type, To: Type]: Type[Transformer[From, To]] =
      fromTC[Transformer[?, ?], Transformer[From, To]](Type[From], Type[To])
    def PartialTransformer[From: Type, To: Type]: Type[PartialTransformer[From, To]] =
      fromTC[PartialTransformer[?, ?], PartialTransformer[From, To]](Type[From], Type[To])
    def Patcher[T: Type, Patch: Type]: Type[Patcher[T, Patch]] =
      fromTC[Patcher[?, ?], Patcher[T, Patch]](Type[T], Type[Patch])

    object PartialResult extends PartialResultModule {
      def apply[T: Type]: Type[partial.Result[T]] = fromTC[partial.Result[?], partial.Result[T]](Type[T])
      def Value[T: Type]: Type[partial.Result.Value[T]] =
        fromTC[partial.Result.Value[?], partial.Result.Value[T]](Type[T])
      val Errors: Type[partial.Result.Errors] = quoted.Type.of[partial.Result.Errors]
    }

    val PreferTotalTransformer: Type[io.scalaland.chimney.dsl.PreferTotalTransformer.type] =
      quoted.Type.of[io.scalaland.chimney.dsl.PreferTotalTransformer.type]
    val PreferPartialTransformer: Type[io.scalaland.chimney.dsl.PreferPartialTransformer.type] =
      quoted.Type.of[io.scalaland.chimney.dsl.PreferPartialTransformer.type]

    val RuntimeDataStore: Type[TransformerDefinitionCommons.RuntimeDataStore] =
      quoted.Type.of[TransformerDefinitionCommons.RuntimeDataStore]

    object TransformerCfg extends TransformerCfgModule {
      val Empty: Type[internal.TransformerCfg.Empty] = quoted.Type.of[internal.TransformerCfg.Empty]
    }

    object TransformerFlags extends TransformerFlagsModule {
      import internal.TransformerFlags.Flag

      val Default: Type[internal.TransformerFlags.Default] = quoted.Type.of[internal.TransformerFlags.Default]

      def Enable[F <: Flag: Type, Flags <: internal.TransformerFlags: Type]
          : Type[internal.TransformerFlags.Enable[F, Flags]] =
        quoted.Type.of[internal.TransformerFlags.Enable[F, Flags]]

      def Disable[F <: Flag: Type, Flags <: internal.TransformerFlags: Type]
          : Type[internal.TransformerFlags.Disable[F, Flags]] =
        quoted.Type.of[internal.TransformerFlags.Disable[F, Flags]]

      object Flags extends FlagsModule {
        val DefaultValues: Type[internal.TransformerFlags.DefaultValues] =
          quoted.Type.of[internal.TransformerFlags.DefaultValues]
        val BeanGetters: Type[internal.TransformerFlags.BeanGetters] =
          quoted.Type.of[internal.TransformerFlags.BeanGetters]
        val BeanSetters: Type[internal.TransformerFlags.BeanSetters] =
          quoted.Type.of[internal.TransformerFlags.BeanSetters]
        val MethodAccessors: Type[internal.TransformerFlags.MethodAccessors] =
          quoted.Type.of[internal.TransformerFlags.MethodAccessors]
        val OptionDefaultsToNone: Type[internal.TransformerFlags.OptionDefaultsToNone] =
          quoted.Type.of[internal.TransformerFlags.OptionDefaultsToNone]
        val DerivationLog: Type[internal.TransformerFlags.DerivationLog] =
          quoted.Type.of[internal.TransformerFlags.DerivationLog]

        def ImplicitConflictResolution[R <: ImplicitTransformerPreference: Type]
            : Type[internal.TransformerFlags.ImplicitConflictResolution[R]] =
          quoted.Type.of[internal.TransformerFlags.ImplicitConflictResolution[R]]

      }
    }
  }
}
