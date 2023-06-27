package io.scalaland.chimney.internal.compiletime

import io.scalaland.chimney.dsl.{ImplicitTransformerPreference, TransformerDefinitionCommons}
import io.scalaland.chimney.partial
import io.scalaland.chimney.internal
import io.scalaland.chimney.{PartialTransformer, Patcher, Transformer}

private[compiletime] trait ChimneyTypesPlatform extends ChimneyTypes { this: DefinitionsPlatform =>

  import c.universe.{internal as _, Transformer as _, *}

  protected object ChimneyType extends ChimneyTypeModule {

    import Type.platformSpecific.fromUntyped

    def Transformer[From: Type, To: Type]: Type[Transformer[From, To]] = weakTypeTag[Transformer[From, To]]

    def PartialTransformer[From: Type, To: Type]: Type[PartialTransformer[From, To]] =
      weakTypeTag[PartialTransformer[From, To]]

    def Patcher[A: Type, Patch: Type]: Type[Patcher[A, Patch]] = weakTypeTag[Patcher[A, Patch]]

    object PartialResult extends PartialResultModule {
      def apply[A: Type]: Type[partial.Result[A]] = weakTypeTag[partial.Result[A]]
      def unapply[A](A: Type[A]): Option[ExistentialType] =
        // Errors has no type parameters, so we need getOrElse(Nothing)
        if (A.tpe.typeConstructor <:< weakTypeOf[partial.Result[?]].typeConstructor)
          Some(A.tpe.typeArgs.headOption.fold(ExistentialType(Type.Nothing))(fromUntyped[Any](_).asExistential))
        else scala.None

      def Value[A: Type]: Type[partial.Result.Value[A]] = weakTypeTag[partial.Result.Value[A]]
      val Errors: Type[partial.Result.Errors] = weakTypeTag[partial.Result.Errors]
    }

    object PathElement extends PathElementModule {
      val tpe: Type[partial.PathElement] = weakTypeTag[partial.PathElement]
      val Accessor: Type[partial.PathElement.Accessor] = weakTypeTag[partial.PathElement.Accessor]
      val Index: Type[partial.PathElement.Index] = weakTypeTag[partial.PathElement.Index]
      val MapKey: Type[partial.PathElement.MapKey] = weakTypeTag[partial.PathElement.MapKey]
      val MapValue: Type[partial.PathElement.MapValue] = weakTypeTag[partial.PathElement.MapValue]
    }

    val PreferTotalTransformer: Type[io.scalaland.chimney.dsl.PreferTotalTransformer.type] =
      weakTypeTag[io.scalaland.chimney.dsl.PreferTotalTransformer.type]
    val PreferPartialTransformer: Type[io.scalaland.chimney.dsl.PreferPartialTransformer.type] =
      weakTypeTag[io.scalaland.chimney.dsl.PreferPartialTransformer.type]

    val RuntimeDataStore: Type[TransformerDefinitionCommons.RuntimeDataStore] =
      weakTypeTag
    object TransformerCfg extends TransformerCfgModule {
      val Empty: Type[internal.TransformerCfg.Empty] = weakTypeTag[internal.TransformerCfg.Empty]
    }

    object TransformerFlags extends TransformerFlagsModule {
      import internal.TransformerFlags.Flag

      val Default: Type[internal.TransformerFlags.Default] = weakTypeTag[internal.TransformerFlags.Default]

      object Enable extends EnableModule {
        def apply[F <: Flag: Type, Flags <: internal.TransformerFlags: Type]
            : Type[internal.TransformerFlags.Enable[F, Flags]] =
          weakTypeTag[internal.TransformerFlags.Enable[F, Flags]]
        def unapply[A](A: Type[A]): Option[(ExistentialType, ExistentialType)] = {
          if (A.tpe.typeConstructor <:< weakTypeOf[internal.TransformerFlags.Enable[?, ?]].typeConstructor)
            Some(fromUntyped(A.tpe.typeArgs(0)).asExistential -> fromUntyped(A.tpe.typeArgs(1)).asExistential)
          else scala.None
        }
      }
      object Disable extends DisableModule {
        def apply[F <: Flag: Type, Flags <: internal.TransformerFlags: Type]
            : Type[internal.TransformerFlags.Disable[F, Flags]] =
          weakTypeTag[internal.TransformerFlags.Disable[F, Flags]]
        def unapply[A](A: Type[A]): Option[(ExistentialType, ExistentialType)] =
          if (A.tpe.typeConstructor <:< weakTypeOf[internal.TransformerFlags.Disable[?, ?]].typeConstructor)
            Some(fromUntyped(A.tpe.typeArgs(0)).asExistential -> fromUntyped(A.tpe.typeArgs(1)).asExistential)
          else scala.None
      }

      object Flags extends FlagsModule {
        val DefaultValues: Type[internal.TransformerFlags.DefaultValues] =
          weakTypeTag[internal.TransformerFlags.DefaultValues]
        val BeanGetters: Type[internal.TransformerFlags.BeanGetters] =
          weakTypeTag[internal.TransformerFlags.BeanGetters]
        val BeanSetters: Type[internal.TransformerFlags.BeanSetters] =
          weakTypeTag[internal.TransformerFlags.BeanSetters]
        val MethodAccessors: Type[internal.TransformerFlags.MethodAccessors] =
          weakTypeTag[internal.TransformerFlags.MethodAccessors]
        val OptionDefaultsToNone: Type[internal.TransformerFlags.OptionDefaultsToNone] =
          weakTypeTag[internal.TransformerFlags.OptionDefaultsToNone]
        object ImplicitConflictResolution extends ImplicitConflictResolutionModule {
          def apply[R <: ImplicitTransformerPreference: Type]
              : Type[internal.TransformerFlags.ImplicitConflictResolution[R]] =
            weakTypeTag[internal.TransformerFlags.ImplicitConflictResolution[R]]
          def unapply[A](A: Type[A]): Option[ExistentialType] =
            if (
              A.tpe.typeConstructor <:< weakTypeOf[
                internal.TransformerFlags.ImplicitConflictResolution[?]
              ].typeConstructor
            )
              Some(fromUntyped(A.tpe.typeArgs.head).asExistential)
            else scala.None
        }
        val MacrosLogging: Type[internal.TransformerFlags.MacrosLogging] =
          weakTypeTag[internal.TransformerFlags.MacrosLogging]
      }
    }
  }
}
