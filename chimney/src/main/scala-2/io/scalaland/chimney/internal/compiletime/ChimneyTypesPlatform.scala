package io.scalaland.chimney.internal.compiletime

import io.scalaland.chimney.dsl.{ImplicitTransformerPreference, TransformerDefinitionCommons}
import io.scalaland.chimney.partial
import io.scalaland.chimney.internal
import io.scalaland.chimney.{PartialTransformer, Patcher, Transformer}

private[compiletime] trait ChimneyTypesPlatform extends ChimneyTypes { this: DefinitionsPlatform =>

  protected object ChimneyType extends ChimneyTypeModule {

    import Type.platformSpecific.{fromUntyped, fromWeak, fromWeakTypeConstructor}, TypeImplicits.*

    def Transformer[From: Type, To: Type]: Type[Transformer[From, To]] =
      fromWeakTypeConstructor[Transformer[?, ?], Transformer[From, To]](Type[From], Type[To])

    def PartialTransformer[From: Type, To: Type]: Type[PartialTransformer[From, To]] =
      fromWeakTypeConstructor[PartialTransformer[?, ?], PartialTransformer[From, To]](Type[From], Type[To])

    def Patcher[A: Type, Patch: Type]: Type[Patcher[A, Patch]] =
      fromWeakTypeConstructor[Patcher[?, ?], Patcher[A, Patch]](Type[A], Type[Patch])

    object PartialResult extends PartialResultModule {
      def apply[A: Type]: Type[partial.Result[A]] =
        fromWeakTypeConstructor[partial.Result[?], partial.Result[A]](Type[A])
      def unapply[A](tpe: Type[A]): Option[ExistentialType] =
        // None has no type parameters, so we need getOrElse(Nothing)
        if (apply[Any] <:< tpe)
          Some(
            tpe.typeArgs.headOption.fold(ExistentialType(Type.Nothing))(inner => fromUntyped[Any](inner).asExistential)
          )
        else scala.None

      def Value[A: Type]: Type[partial.Result.Value[A]] =
        fromWeakTypeConstructor[partial.Result.Value[?], partial.Result.Value[A]](Type[A])
      val Errors: Type[partial.Result.Errors] =
        fromWeak[partial.Result.Errors]
    }

    object PathElement extends PathElementModule {
      val Accessor: Type[partial.PathElement.Accessor] = fromWeak[partial.PathElement.Accessor]
      val Index: Type[partial.PathElement.Index] = fromWeak[partial.PathElement.Index]
      val MapKey: Type[partial.PathElement.MapKey] = fromWeak[partial.PathElement.MapKey]
      val MapValue: Type[partial.PathElement.MapValue] = fromWeak[partial.PathElement.MapValue]
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
        val MacrosLogging: Type[internal.TransformerFlags.MacrosLogging] =
          fromWeak[internal.TransformerFlags.MacrosLogging]

        def ImplicitConflictResolution[R <: ImplicitTransformerPreference: Type]
            : Type[internal.TransformerFlags.ImplicitConflictResolution[R]] =
          fromWeak[internal.TransformerFlags.ImplicitConflictResolution[R]]
      }
    }

  }
}
