package io.scalaland.chimney.internal.compiletime

import io.scalaland.chimney.{PartialTransformer, Patcher, Transformer}
import io.scalaland.chimney.dsl.{ImplicitTransformerPreference, TransformerDefinitionCommons}
import io.scalaland.chimney.internal.runtime
import io.scalaland.chimney.partial

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
      val Empty: Type[runtime.TransformerCfg.Empty] = weakTypeTag[runtime.TransformerCfg.Empty]
      object FieldConst extends FieldConstModule {
        def apply[Name <: String: Type, Cfg <: runtime.TransformerCfg: Type]
            : Type[runtime.TransformerCfg.FieldConst[Name, Cfg]] =
          weakTypeTag[runtime.TransformerCfg.FieldConst[Name, Cfg]]
        def unapply[A](A: Type[A]): Option[
          (ExistentialType.UpperBounded[String], ExistentialType.UpperBounded[runtime.TransformerCfg])
        ] =
          if (A.tpe.typeConstructor <:< weakTypeOf[runtime.TransformerCfg.FieldConst[?, ?]].typeConstructor)
            Some(
              (
                fromUntyped[String](A.tpe.typeArgs(0)).asExistentialUpperBounded[String],
                fromUntyped[runtime.TransformerCfg](A.tpe.typeArgs(1))
                  .asExistentialUpperBounded[runtime.TransformerCfg]
              )
            )
          else scala.None
      }
      object FieldConstPartial extends FieldConstPartialModule {
        def apply[Name <: String: Type, Cfg <: runtime.TransformerCfg: Type]
            : Type[runtime.TransformerCfg.FieldConstPartial[Name, Cfg]] =
          weakTypeTag[runtime.TransformerCfg.FieldConstPartial[Name, Cfg]]
        def unapply[A](A: Type[A]): Option[
          (ExistentialType.UpperBounded[String], ExistentialType.UpperBounded[runtime.TransformerCfg])
        ] =
          if (A.tpe.typeConstructor <:< weakTypeOf[runtime.TransformerCfg.FieldConstPartial[?, ?]].typeConstructor)
            Some(
              (
                fromUntyped[String](A.tpe.typeArgs(0)).asExistentialUpperBounded[String],
                fromUntyped[runtime.TransformerCfg](A.tpe.typeArgs(1))
                  .asExistentialUpperBounded[runtime.TransformerCfg]
              )
            )
          else scala.None
      }
      object FieldComputed extends FieldComputedModule {
        def apply[Name <: String: Type, Cfg <: runtime.TransformerCfg: Type]
            : Type[runtime.TransformerCfg.FieldComputed[Name, Cfg]] =
          weakTypeTag[runtime.TransformerCfg.FieldComputed[Name, Cfg]]
        def unapply[A](A: Type[A]): Option[
          (ExistentialType.UpperBounded[String], ExistentialType.UpperBounded[runtime.TransformerCfg])
        ] =
          if (A.tpe.typeConstructor <:< weakTypeOf[runtime.TransformerCfg.FieldComputed[?, ?]].typeConstructor)
            Some(
              (
                fromUntyped[String](A.tpe.typeArgs(0)).asExistentialUpperBounded[String],
                fromUntyped[runtime.TransformerCfg](A.tpe.typeArgs(1))
                  .asExistentialUpperBounded[runtime.TransformerCfg]
              )
            )
          else scala.None
      }
      object FieldComputedPartial extends FieldComputedPartialModule {
        def apply[Name <: String: Type, Cfg <: runtime.TransformerCfg: Type]
            : Type[runtime.TransformerCfg.FieldComputedPartial[Name, Cfg]] =
          weakTypeTag[runtime.TransformerCfg.FieldComputedPartial[Name, Cfg]]
        def unapply[A](A: Type[A]): Option[
          (ExistentialType.UpperBounded[String], ExistentialType.UpperBounded[runtime.TransformerCfg])
        ] =
          if (A.tpe.typeConstructor <:< weakTypeOf[runtime.TransformerCfg.FieldComputedPartial[?, ?]].typeConstructor)
            Some(
              (
                fromUntyped[String](A.tpe.typeArgs(0)).asExistentialUpperBounded[String],
                fromUntyped[runtime.TransformerCfg](A.tpe.typeArgs(1))
                  .asExistentialUpperBounded[runtime.TransformerCfg]
              )
            )
          else scala.None
      }
      object FieldRelabelled extends FieldRelabelledModule {
        def apply[FromName <: String: Type, ToName <: String: Type, Cfg <: runtime.TransformerCfg: Type]
            : Type[runtime.TransformerCfg.FieldRelabelled[FromName, ToName, Cfg]] =
          weakTypeTag[runtime.TransformerCfg.FieldRelabelled[FromName, ToName, Cfg]]
        def unapply[A](A: Type[A]): Option[
          (
              ExistentialType.UpperBounded[String],
              ExistentialType.UpperBounded[String],
              ExistentialType.UpperBounded[runtime.TransformerCfg]
          )
        ] =
          if (A.tpe.typeConstructor <:< weakTypeOf[runtime.TransformerCfg.FieldRelabelled[?, ?, ?]].typeConstructor)
            Some(
              (
                fromUntyped[String](A.tpe.typeArgs(0)).asExistentialUpperBounded[String],
                fromUntyped[String](A.tpe.typeArgs(1)).asExistentialUpperBounded[String],
                fromUntyped[runtime.TransformerCfg](A.tpe.typeArgs(2))
                  .asExistentialUpperBounded[runtime.TransformerCfg]
              )
            )
          else scala.None
      }
      object CoproductInstance extends CoproductInstanceModule {
        def apply[InstType: Type, TargetType: Type, Cfg <: runtime.TransformerCfg: Type]
            : Type[runtime.TransformerCfg.CoproductInstance[InstType, TargetType, Cfg]] =
          weakTypeTag[runtime.TransformerCfg.CoproductInstance[InstType, TargetType, Cfg]]
        def unapply[A](A: Type[A]): Option[
          (
              ExistentialType,
              ExistentialType,
              ExistentialType.UpperBounded[runtime.TransformerCfg]
          )
        ] =
          if (A.tpe.typeConstructor <:< weakTypeOf[runtime.TransformerCfg.CoproductInstance[?, ?, ?]].typeConstructor)
            Some(
              (
                fromUntyped[String](A.tpe.typeArgs(0)).asExistential,
                fromUntyped[String](A.tpe.typeArgs(1)).asExistential,
                fromUntyped[runtime.TransformerCfg](A.tpe.typeArgs(2))
                  .asExistentialUpperBounded[runtime.TransformerCfg]
              )
            )
          else scala.None
      }
      object CoproductInstancePartial extends CoproductInstancePartialModule {
        def apply[InstType: Type, TargetType: Type, Cfg <: runtime.TransformerCfg: Type]
            : Type[runtime.TransformerCfg.CoproductInstancePartial[InstType, TargetType, Cfg]] =
          weakTypeTag[runtime.TransformerCfg.CoproductInstancePartial[InstType, TargetType, Cfg]]
        def unapply[A](A: Type[A]): Option[
          (
              ExistentialType,
              ExistentialType,
              ExistentialType.UpperBounded[runtime.TransformerCfg]
          )
        ] =
          if (
            A.tpe.typeConstructor <:< weakTypeOf[
              runtime.TransformerCfg.CoproductInstancePartial[?, ?, ?]
            ].typeConstructor
          )
            Some(
              (
                fromUntyped[String](A.tpe.typeArgs(0)).asExistential,
                fromUntyped[String](A.tpe.typeArgs(1)).asExistential,
                fromUntyped[runtime.TransformerCfg](A.tpe.typeArgs(2))
                  .asExistentialUpperBounded[runtime.TransformerCfg]
              )
            )
          else scala.None
      }
    }

    object TransformerFlags extends TransformerFlagsModule {
      val Default: Type[runtime.TransformerFlags.Default] = weakTypeTag[runtime.TransformerFlags.Default]

      object Enable extends EnableModule {
        def apply[F <: runtime.TransformerFlags.Flag: Type, Flags <: runtime.TransformerFlags: Type]
            : Type[runtime.TransformerFlags.Enable[F, Flags]] =
          weakTypeTag[runtime.TransformerFlags.Enable[F, Flags]]
        def unapply[A](A: Type[A]): Option[
          (
              ExistentialType.UpperBounded[runtime.TransformerFlags.Flag],
              ExistentialType.UpperBounded[runtime.TransformerFlags]
          )
        ] = {
          if (A.tpe.typeConstructor <:< weakTypeOf[runtime.TransformerFlags.Enable[?, ?]].typeConstructor)
            Some(
              (
                fromUntyped[runtime.TransformerFlags.Flag](A.tpe.typeArgs(0))
                  .asExistentialUpperBounded[runtime.TransformerFlags.Flag],
                fromUntyped[runtime.TransformerFlags](A.tpe.typeArgs(1))
                  .asExistentialUpperBounded[runtime.TransformerFlags]
              )
            )
          else scala.None
        }
      }
      object Disable extends DisableModule {
        def apply[F <: runtime.TransformerFlags.Flag: Type, Flags <: runtime.TransformerFlags: Type]
            : Type[runtime.TransformerFlags.Disable[F, Flags]] =
          weakTypeTag[runtime.TransformerFlags.Disable[F, Flags]]
        def unapply[A](A: Type[A]): Option[
          (
              ExistentialType.UpperBounded[runtime.TransformerFlags.Flag],
              ExistentialType.UpperBounded[runtime.TransformerFlags]
          )
        ] =
          if (A.tpe.typeConstructor <:< weakTypeOf[runtime.TransformerFlags.Disable[?, ?]].typeConstructor)
            Some(
              (
                fromUntyped[runtime.TransformerFlags.Flag](A.tpe.typeArgs(0))
                  .asExistentialUpperBounded[runtime.TransformerFlags.Flag],
                fromUntyped[runtime.TransformerFlags](A.tpe.typeArgs(1))
                  .asExistentialUpperBounded[runtime.TransformerFlags]
              )
            )
          else scala.None
      }

      object Flags extends FlagsModule {
        val DefaultValues: Type[runtime.TransformerFlags.DefaultValues] =
          weakTypeTag[runtime.TransformerFlags.DefaultValues]
        val BeanGetters: Type[runtime.TransformerFlags.BeanGetters] =
          weakTypeTag[runtime.TransformerFlags.BeanGetters]
        val BeanSetters: Type[runtime.TransformerFlags.BeanSetters] =
          weakTypeTag[runtime.TransformerFlags.BeanSetters]
        val MethodAccessors: Type[runtime.TransformerFlags.MethodAccessors] =
          weakTypeTag[runtime.TransformerFlags.MethodAccessors]
        val OptionDefaultsToNone: Type[runtime.TransformerFlags.OptionDefaultsToNone] =
          weakTypeTag[runtime.TransformerFlags.OptionDefaultsToNone]
        object ImplicitConflictResolution extends ImplicitConflictResolutionModule {
          def apply[R <: ImplicitTransformerPreference: Type]
              : Type[runtime.TransformerFlags.ImplicitConflictResolution[R]] =
            weakTypeTag[runtime.TransformerFlags.ImplicitConflictResolution[R]]
          def unapply[A](A: Type[A]): Option[ExistentialType.UpperBounded[ImplicitTransformerPreference]] =
            if (
              A.tpe.typeConstructor <:< weakTypeOf[
                runtime.TransformerFlags.ImplicitConflictResolution[?]
              ].typeConstructor
            )
              Some(
                fromUntyped[ImplicitTransformerPreference](A.tpe.typeArgs.head)
                  .asExistentialUpperBounded[ImplicitTransformerPreference]
              )
            else scala.None
        }
        val MacrosLogging: Type[runtime.TransformerFlags.MacrosLogging] =
          weakTypeTag[runtime.TransformerFlags.MacrosLogging]
      }
    }

    object PatcherCfg extends PatcherCfgModule {
      override val Empty: Type[runtime.PatcherCfg.Empty] = weakTypeTag[runtime.PatcherCfg.Empty]

      object IgnoreRedundantPatcherFields extends IgnoreRedundantPatcherFieldsModule {
        override def apply[Cfg <: runtime.PatcherCfg: Type]
            : Type[runtime.PatcherCfg.IgnoreRedundantPatcherFields[Cfg]] =
          weakTypeTag[runtime.PatcherCfg.IgnoreRedundantPatcherFields[Cfg]]

        override def unapply[A](A: Type[A]): Option[ExistentialType.UpperBounded[runtime.PatcherCfg]] =
          if (A.tpe.typeConstructor <:< weakTypeOf[runtime.PatcherCfg.IgnoreRedundantPatcherFields[?]].typeConstructor)
            Some(fromUntyped[runtime.PatcherCfg](A.tpe.typeArgs.head).asExistentialUpperBounded[runtime.PatcherCfg])
          else
            scala.None
      }

      object IgnoreNoneInPatch extends IgnoreNoneInPatchModule {
        override def apply[Cfg <: runtime.PatcherCfg: Type]: Type[runtime.PatcherCfg.IgnoreNoneInPatch[Cfg]] =
          weakTypeTag[runtime.PatcherCfg.IgnoreNoneInPatch[Cfg]]

        override def unapply[A](A: Type[A]): Option[ExistentialType.UpperBounded[runtime.PatcherCfg]] =
          if (A.tpe.typeConstructor <:< weakTypeOf[runtime.PatcherCfg.IgnoreNoneInPatch[?]].typeConstructor)
            Some(fromUntyped[runtime.PatcherCfg](A.tpe.typeArgs.head).asExistentialUpperBounded[runtime.PatcherCfg])
          else
            scala.None
      }

      object MacrosLogging extends MacrosLoggingModule {
        override def apply[Cfg <: runtime.PatcherCfg: Type]: Type[runtime.PatcherCfg.MacrosLogging[Cfg]] =
          weakTypeTag[runtime.PatcherCfg.MacrosLogging[Cfg]]

        override def unapply[A](A: Type[A]): Option[ExistentialType.UpperBounded[runtime.PatcherCfg]] =
          if (A.tpe.typeConstructor <:< weakTypeOf[runtime.PatcherCfg.MacrosLogging[?]].typeConstructor)
            Some(fromUntyped[runtime.PatcherCfg](A.tpe.typeArgs.head).asExistentialUpperBounded[runtime.PatcherCfg])
          else
            scala.None
      }
    }
  }
}
