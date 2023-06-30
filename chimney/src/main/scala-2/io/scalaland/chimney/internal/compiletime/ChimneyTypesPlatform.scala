package io.scalaland.chimney.internal.compiletime

import io.scalaland.chimney.dsl.{ImplicitTransformerPreference, TransformerDefinitionCommons}
import io.scalaland.chimney.partial
import io.scalaland.chimney.internal
import io.scalaland.chimney.{PartialTransformer, Patcher, Transformer}

private[compiletime] trait ChimneyTypesPlatform extends ChimneyTypes { this: ChimneyDefinitionsPlatform =>

  import c.universe.{internal as _, Name as _, Transformer as _, *}

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
      weakTypeTag[TransformerDefinitionCommons.RuntimeDataStore]

    object TransformerCfg extends TransformerCfgModule {
      val Empty: Type[internal.TransformerCfg.Empty] = weakTypeTag[internal.TransformerCfg.Empty]
      object FieldConst extends FieldConstModule {
        def apply[Name <: String: Type, C <: internal.TransformerCfg: Type]
            : Type[internal.TransformerCfg.FieldConst[Name, C]] =
          weakTypeTag[internal.TransformerCfg.FieldConst[Name, C]]
        def unapply[A](A: Type[A]): Option[
          (ExistentialType.UpperBounded[String], ExistentialType.UpperBounded[internal.TransformerCfg])
        ] =
          if (A.tpe.typeConstructor <:< weakTypeOf[internal.TransformerCfg.FieldConst[?, ?]].typeConstructor)
            Some(
              (
                fromUntyped[String](A.tpe.typeArgs(0)).asExistentialUpperBounded[String],
                fromUntyped[internal.TransformerCfg](A.tpe.typeArgs(1))
                  .asExistentialUpperBounded[internal.TransformerCfg]
              )
            )
          else scala.None
      }
      object FieldConstPartial extends FieldConstPartialModule {
        def apply[Name <: String: Type, C <: internal.TransformerCfg: Type]
            : Type[internal.TransformerCfg.FieldConstPartial[Name, C]] =
          weakTypeTag[internal.TransformerCfg.FieldConstPartial[Name, C]]
        def unapply[A](A: Type[A]): Option[
          (ExistentialType.UpperBounded[String], ExistentialType.UpperBounded[internal.TransformerCfg])
        ] =
          if (A.tpe.typeConstructor <:< weakTypeOf[internal.TransformerCfg.FieldConstPartial[?, ?]].typeConstructor)
            Some(
              (
                fromUntyped[String](A.tpe.typeArgs(0)).asExistentialUpperBounded[String],
                fromUntyped[internal.TransformerCfg](A.tpe.typeArgs(1))
                  .asExistentialUpperBounded[internal.TransformerCfg]
              )
            )
          else scala.None
      }
      object FieldComputed extends FieldComputedModule {
        def apply[Name <: String: Type, C <: internal.TransformerCfg: Type]
            : Type[internal.TransformerCfg.FieldComputed[Name, C]] =
          weakTypeTag[internal.TransformerCfg.FieldComputed[Name, C]]
        def unapply[A](A: Type[A]): Option[
          (ExistentialType.UpperBounded[String], ExistentialType.UpperBounded[internal.TransformerCfg])
        ] =
          if (A.tpe.typeConstructor <:< weakTypeOf[internal.TransformerCfg.FieldComputed[?, ?]].typeConstructor)
            Some(
              (
                fromUntyped[String](A.tpe.typeArgs(0)).asExistentialUpperBounded[String],
                fromUntyped[internal.TransformerCfg](A.tpe.typeArgs(1))
                  .asExistentialUpperBounded[internal.TransformerCfg]
              )
            )
          else scala.None
      }
      object FieldComputedPartial extends FieldComputedPartialModule {
        def apply[Name <: String: Type, C <: internal.TransformerCfg: Type]
            : Type[internal.TransformerCfg.FieldComputedPartial[Name, C]] =
          weakTypeTag[internal.TransformerCfg.FieldComputedPartial[Name, C]]
        def unapply[A](A: Type[A]): Option[
          (ExistentialType.UpperBounded[String], ExistentialType.UpperBounded[internal.TransformerCfg])
        ] =
          if (A.tpe.typeConstructor <:< weakTypeOf[internal.TransformerCfg.FieldComputedPartial[?, ?]].typeConstructor)
            Some(
              (
                fromUntyped[String](A.tpe.typeArgs(0)).asExistentialUpperBounded[String],
                fromUntyped[internal.TransformerCfg](A.tpe.typeArgs(1))
                  .asExistentialUpperBounded[internal.TransformerCfg]
              )
            )
          else scala.None
      }
      object FieldRelabelled extends FieldRelabelledModule {
        def apply[FromName <: String: Type, ToName <: String: Type, C <: internal.TransformerCfg: Type]
            : Type[internal.TransformerCfg.FieldRelabelled[FromName, ToName, C]] =
          weakTypeTag[internal.TransformerCfg.FieldRelabelled[FromName, ToName, C]]
        def unapply[A](A: Type[A]): Option[
          (
              ExistentialType.UpperBounded[String],
              ExistentialType.UpperBounded[String],
              ExistentialType.UpperBounded[internal.TransformerCfg]
          )
        ] =
          if (A.tpe.typeConstructor <:< weakTypeOf[internal.TransformerCfg.FieldRelabelled[?, ?, ?]].typeConstructor)
            Some(
              (
                fromUntyped[String](A.tpe.typeArgs(0)).asExistentialUpperBounded[String],
                fromUntyped[String](A.tpe.typeArgs(1)).asExistentialUpperBounded[String],
                fromUntyped[internal.TransformerCfg](A.tpe.typeArgs(2))
                  .asExistentialUpperBounded[internal.TransformerCfg]
              )
            )
          else scala.None
      }
      object CoproductInstance extends CoproductInstanceModule {
        def apply[InstType: Type, TargetType: Type, C <: internal.TransformerCfg: Type]
            : Type[internal.TransformerCfg.CoproductInstance[InstType, TargetType, C]] =
          weakTypeTag[internal.TransformerCfg.CoproductInstance[InstType, TargetType, C]]
        def unapply[A](A: Type[A]): Option[
          (
              ExistentialType,
              ExistentialType,
              ExistentialType.UpperBounded[internal.TransformerCfg]
          )
        ] =
          if (A.tpe.typeConstructor <:< weakTypeOf[internal.TransformerCfg.CoproductInstance[?, ?, ?]].typeConstructor)
            Some(
              (
                fromUntyped[String](A.tpe.typeArgs(0)).asExistential,
                fromUntyped[String](A.tpe.typeArgs(1)).asExistential,
                fromUntyped[internal.TransformerCfg](A.tpe.typeArgs(2))
                  .asExistentialUpperBounded[internal.TransformerCfg]
              )
            )
          else scala.None
      }
      object CoproductInstancePartial extends CoproductInstancePartialModule {
        def apply[InstType: Type, TargetType: Type, C <: internal.TransformerCfg: Type]
            : Type[internal.TransformerCfg.CoproductInstancePartial[InstType, TargetType, C]] =
          weakTypeTag[internal.TransformerCfg.CoproductInstancePartial[InstType, TargetType, C]]
        def unapply[A](A: Type[A]): Option[
          (
              ExistentialType,
              ExistentialType,
              ExistentialType.UpperBounded[internal.TransformerCfg]
          )
        ] =
          if (
            A.tpe.typeConstructor <:< weakTypeOf[
              internal.TransformerCfg.CoproductInstancePartial[?, ?, ?]
            ].typeConstructor
          )
            Some(
              (
                fromUntyped[String](A.tpe.typeArgs(0)).asExistential,
                fromUntyped[String](A.tpe.typeArgs(1)).asExistential,
                fromUntyped[internal.TransformerCfg](A.tpe.typeArgs(2))
                  .asExistentialUpperBounded[internal.TransformerCfg]
              )
            )
          else scala.None
      }
    }

    object TransformerFlags extends TransformerFlagsModule {
      val Default: Type[internal.TransformerFlags.Default] = weakTypeTag[internal.TransformerFlags.Default]

      object Enable extends EnableModule {
        def apply[F <: internal.TransformerFlags.Flag: Type, Flags <: internal.TransformerFlags: Type]
            : Type[internal.TransformerFlags.Enable[F, Flags]] =
          weakTypeTag[internal.TransformerFlags.Enable[F, Flags]]
        def unapply[A](A: Type[A]): Option[
          (
              ExistentialType.UpperBounded[internal.TransformerFlags.Flag],
              ExistentialType.UpperBounded[internal.TransformerFlags]
          )
        ] = {
          if (A.tpe.typeConstructor <:< weakTypeOf[internal.TransformerFlags.Enable[?, ?]].typeConstructor)
            Some(
              (
                fromUntyped[internal.TransformerFlags.Flag](A.tpe.typeArgs(0))
                  .asExistentialUpperBounded[internal.TransformerFlags.Flag],
                fromUntyped[internal.TransformerFlags](A.tpe.typeArgs(1))
                  .asExistentialUpperBounded[internal.TransformerFlags]
              )
            )
          else scala.None
        }
      }
      object Disable extends DisableModule {
        def apply[F <: internal.TransformerFlags.Flag: Type, Flags <: internal.TransformerFlags: Type]
            : Type[internal.TransformerFlags.Disable[F, Flags]] =
          weakTypeTag[internal.TransformerFlags.Disable[F, Flags]]
        def unapply[A](A: Type[A]): Option[
          (
              ExistentialType.UpperBounded[internal.TransformerFlags.Flag],
              ExistentialType.UpperBounded[internal.TransformerFlags]
          )
        ] =
          if (A.tpe.typeConstructor <:< weakTypeOf[internal.TransformerFlags.Disable[?, ?]].typeConstructor)
            Some(
              (
                fromUntyped[internal.TransformerFlags.Flag](A.tpe.typeArgs(0))
                  .asExistentialUpperBounded[internal.TransformerFlags.Flag],
                fromUntyped[internal.TransformerFlags](A.tpe.typeArgs(1))
                  .asExistentialUpperBounded[internal.TransformerFlags]
              )
            )
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
          def unapply[A](A: Type[A]): Option[ExistentialType.UpperBounded[ImplicitTransformerPreference]] =
            if (
              A.tpe.typeConstructor <:< weakTypeOf[
                internal.TransformerFlags.ImplicitConflictResolution[?]
              ].typeConstructor
            )
              Some(
                fromUntyped[ImplicitTransformerPreference](A.tpe.typeArgs.head)
                  .asExistentialUpperBounded[ImplicitTransformerPreference]
              )
            else scala.None
        }
        val MacrosLogging: Type[internal.TransformerFlags.MacrosLogging] =
          weakTypeTag[internal.TransformerFlags.MacrosLogging]
      }
    }
  }
}
