package io.scalaland.chimney.internal.compiletime

import io.scalaland.chimney.dsl as dsls
import io.scalaland.chimney.internal
import io.scalaland.chimney.{partial, PartialTransformer, Patcher, Transformer}
import io.scalaland.chimney.dsl.{ImplicitTransformerPreference, TransformerDefinitionCommons}

import scala.quoted

private[compiletime] trait ChimneyTypesPlatform extends ChimneyTypes { this: DefinitionsPlatform =>

  import quotes.*, quotes.reflect.*

  object ChimneyType extends ChimneyTypeModule {

    def Transformer[From: Type, To: Type]: Type[Transformer[From, To]] = quoted.Type.of[Transformer[From, To]]
    def PartialTransformer[From: Type, To: Type]: Type[PartialTransformer[From, To]] =
      quoted.Type.of[PartialTransformer[From, To]]
    def Patcher[T: Type, Patch: Type]: Type[Patcher[T, Patch]] = quoted.Type.of[Patcher[T, Patch]]

    object PartialResult extends PartialResultModule {
      def apply[T: Type]: Type[partial.Result[T]] = quoted.Type.of[partial.Result[T]]
      def unapply[T](tpe: Type[T]): Option[ExistentialType] = tpe match {
        case '[partial.Result[inner]] => Some(Type[inner].asExistential)
        case _                        => scala.None
      }

      def Value[T: Type]: Type[partial.Result.Value[T]] = quoted.Type.of[partial.Result.Value[T]]
      val Errors: Type[partial.Result.Errors] = quoted.Type.of[partial.Result.Errors]
    }

    object PathElement extends PathElementModule {
      val tpe: Type[partial.PathElement] = quoted.Type.of[partial.PathElement]
      val Accessor: Type[partial.PathElement.Accessor] = quoted.Type.of[partial.PathElement.Accessor]
      val Index: Type[partial.PathElement.Index] = quoted.Type.of[partial.PathElement.Index]
      val MapKey: Type[partial.PathElement.MapKey] = quoted.Type.of[partial.PathElement.MapKey]
      val MapValue: Type[partial.PathElement.MapValue] = quoted.Type.of[partial.PathElement.MapValue]
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
      object Enable extends EnableModule {
        def apply[F <: internal.TransformerFlags.Flag: Type, Flags <: internal.TransformerFlags: Type]
            : Type[internal.TransformerFlags.Enable[F, Flags]] =
          quoted.Type.of[internal.TransformerFlags.Enable[F, Flags]]
        def unapply[A](tpe: Type[A]): Option[(ExistentialType, ExistentialType)] = tpe match
          case '[internal.TransformerFlags.Enable[f, flags]] =>
            Some(Type[f].asExistential -> Type[flags].asExistential)
          case _ => scala.None
      }
      object Disable extends DisableModule {
        def apply[F <: internal.TransformerFlags.Flag: Type, Flags <: internal.TransformerFlags: Type]
            : Type[internal.TransformerFlags.Disable[F, Flags]] =
          quoted.Type.of[internal.TransformerFlags.Disable[F, Flags]]
        def unapply[A](tpe: Type[A]): Option[(ExistentialType, ExistentialType)] = tpe match
          case '[internal.TransformerFlags.Disable[f, flags]] =>
            Some(Type[f].asExistential -> Type[flags].asExistential)
          case _ => scala.None
      }

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
        object ImplicitConflictResolution extends ImplicitConflictResolutionModule {
          def apply[R <: ImplicitTransformerPreference: Type]
              : Type[internal.TransformerFlags.ImplicitConflictResolution[R]] =
            quoted.Type.of[internal.TransformerFlags.ImplicitConflictResolution[R]]
          def unapply[A](tpe: Type[A]): Option[ExistentialType] = tpe match
            case '[internal.TransformerFlags.ImplicitConflictResolution[r]] => Some(Type[r].asExistential)
            case _                                                          => scala.None
        }
        val MacrosLogging: Type[internal.TransformerFlags.MacrosLogging] =
          quoted.Type.of[internal.TransformerFlags.MacrosLogging]
      }
    }
  }
}
