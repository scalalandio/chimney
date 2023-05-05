package io.scalaland.chimney.internal.compiletime

import io.scalaland.chimney.dsl.{ImplicitTransformerPreference, TransformerDefinitionCommons}
import io.scalaland.chimney.partial
import io.scalaland.chimney.internal
import io.scalaland.chimney.{PartialTransformer, Patcher, Transformer}

private[compiletime] trait ChimneyTypesPlatform extends ChimneyTypes { this: DefinitionsPlatform =>

  import c.universe.{internal as _, Transformer as _}

  object ChimneyType extends ChimneyTypeModule {
    import typeUtils.*

    def Transformer[From: Type, To: Type]: Type[Transformer[From, To]] =
      fromWeakTC[Transformer[?, ?], Transformer[From, To]](Type[From], Type[To])

    def PartialTransformer[From: Type, To: Type]: Type[PartialTransformer[From, To]] =
      fromWeakTC[PartialTransformer[?, ?], PartialTransformer[From, To]](Type[From], Type[To])

    def Patcher[T: Type, Patch: Type]: Type[Patcher[T, Patch]] =
      fromWeakTC[Patcher[?, ?], Patcher[T, Patch]](Type[T], Type[Patch])

    object PartialResult extends PartialResultModule {
      def apply[T: Type]: Type[partial.Result[T]] =
        fromWeakTC[partial.Result[?], partial.Result[T]](Type[T])
      def Value[T: Type]: Type[partial.Result.Value[T]] =
        fromWeakTC[partial.Result.Value[?], partial.Result.Value[T]](Type[T])
      val Errors: Type[partial.Result.Errors] =
        fromWeak[partial.Result.Errors]
    }

    val PreferTotalTransformer: Type[io.scalaland.chimney.dsl.PreferTotalTransformer.type] =
      fromWeak[io.scalaland.chimney.dsl.PreferTotalTransformer.type]
    val PreferPartialTransformer: Type[io.scalaland.chimney.dsl.PreferPartialTransformer.type] =
      fromWeak[io.scalaland.chimney.dsl.PreferPartialTransformer.type]

    val RuntimeDataStore: Type[TransformerDefinitionCommons.RuntimeDataStore] =
      fromWeak[TransformerDefinitionCommons.RuntimeDataStore]

    object TransformerCfg extends TransformerCfgModule {
      val Empty: Type[internal.TransformerCfg.Empty] = fromWeak[internal.TransformerCfg.Empty]
    }

    object TransformerFlags extends TransformerFlagsModule {
      import internal.TransformerFlags.Flag

      override val Default: Type[internal.TransformerFlags.Default] =
        fromWeak[internal.TransformerFlags.Default]

      def Enable[F <: Flag: Type, Flags <: internal.TransformerFlags: Type]
          : Type[internal.TransformerFlags.Enable[F, Flags]] =
        fromWeak[internal.TransformerFlags.Enable[F, Flags]]

      def Disable[F <: Flag: Type, Flags <: internal.TransformerFlags: Type]
          : Type[internal.TransformerFlags.Disable[F, Flags]] =
        fromWeak[internal.TransformerFlags.Disable[F, Flags]]

      object Flags extends FlagsModule {
        val DefaultValues: Type[internal.TransformerFlags.DefaultValues] =
          fromWeak[internal.TransformerFlags.DefaultValues]
        val BeanGetters: Type[internal.TransformerFlags.BeanGetters] = fromWeak[internal.TransformerFlags.BeanGetters]
        val BeanSetters: Type[internal.TransformerFlags.BeanSetters] = fromWeak[internal.TransformerFlags.BeanSetters]
        val MethodAccessors: Type[internal.TransformerFlags.MethodAccessors] =
          fromWeak[internal.TransformerFlags.MethodAccessors]
        val OptionDefaultsToNone: Type[internal.TransformerFlags.OptionDefaultsToNone] =
          fromWeak[internal.TransformerFlags.OptionDefaultsToNone]
        val DerivationLog: Type[internal.TransformerFlags.DerivationLog] =
          fromWeak[internal.TransformerFlags.DerivationLog]

        def ImplicitConflictResolution[R <: ImplicitTransformerPreference: Type]
            : Type[internal.TransformerFlags.ImplicitConflictResolution[R]] =
          fromWeak[internal.TransformerFlags.ImplicitConflictResolution[R]]
      }
    }
  }
}
