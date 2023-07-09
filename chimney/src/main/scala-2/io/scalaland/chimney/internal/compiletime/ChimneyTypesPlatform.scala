package io.scalaland.chimney.internal.compiletime

import io.scalaland.chimney.{PartialTransformer, Patcher, Transformer}
import io.scalaland.chimney.dsl as dsls
import io.scalaland.chimney.internal.runtime
import io.scalaland.chimney.partial

private[compiletime] trait ChimneyTypesPlatform extends ChimneyTypes { this: ChimneyDefinitionsPlatform =>

  import c.universe.{internal as _, Name as _, Transformer as _, *}

  protected object ChimneyType extends ChimneyTypeModule {

    import Type.platformSpecific.*

    def Transformer[From: Type, To: Type]: Type[Transformer[From, To]] = weakTypeTag[Transformer[From, To]]

    def PartialTransformer[From: Type, To: Type]: Type[PartialTransformer[From, To]] =
      weakTypeTag[PartialTransformer[From, To]]

    def Patcher[A: Type, Patch: Type]: Type[Patcher[A, Patch]] = weakTypeTag[Patcher[A, Patch]]

    object PartialResult extends PartialResultModule {
      def apply[A: Type]: Type[partial.Result[A]] = weakTypeTag[partial.Result[A]]
      def unapply[A](A: Type[A]): Option[??] =
        if (A <:< Errors) Some(ExistentialType(Type.Nothing))
        else if (A.isCtor[partial.Result[?]]) Some(A.param(0))
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

    object TransformerCfg extends TransformerCfgModule {
      val Empty: Type[runtime.TransformerCfg.Empty] = weakTypeTag[runtime.TransformerCfg.Empty]
      object FieldConst extends FieldConstModule {
        def apply[Name <: String: Type, Cfg <: runtime.TransformerCfg: Type]
            : Type[runtime.TransformerCfg.FieldConst[Name, Cfg]] =
          weakTypeTag[runtime.TransformerCfg.FieldConst[Name, Cfg]]
        def unapply[A](A: Type[A]): Option[(?<[String], ?<[runtime.TransformerCfg])] =
          if (A.isCtor[runtime.TransformerCfg.FieldConst[?, ?]])
            Some(A.param_<[String](0) -> A.param_<[runtime.TransformerCfg](1))
          else scala.None
      }
      object FieldConstPartial extends FieldConstPartialModule {
        def apply[Name <: String: Type, Cfg <: runtime.TransformerCfg: Type]
            : Type[runtime.TransformerCfg.FieldConstPartial[Name, Cfg]] =
          weakTypeTag[runtime.TransformerCfg.FieldConstPartial[Name, Cfg]]
        def unapply[A](A: Type[A]): Option[(?<[String], ?<[runtime.TransformerCfg])] =
          if (A.isCtor[runtime.TransformerCfg.FieldConstPartial[?, ?]])
            Some(A.param_<[String](0) -> A.param_<[runtime.TransformerCfg](1))
          else scala.None
      }
      object FieldComputed extends FieldComputedModule {
        def apply[Name <: String: Type, Cfg <: runtime.TransformerCfg: Type]
            : Type[runtime.TransformerCfg.FieldComputed[Name, Cfg]] =
          weakTypeTag[runtime.TransformerCfg.FieldComputed[Name, Cfg]]
        def unapply[A](A: Type[A]): Option[(?<[String], ?<[runtime.TransformerCfg])] =
          if (A.isCtor[runtime.TransformerCfg.FieldComputed[?, ?]])
            Some(A.param_<[String](0) -> A.param_<[runtime.TransformerCfg](1))
          else scala.None
      }
      object FieldComputedPartial extends FieldComputedPartialModule {
        def apply[Name <: String: Type, Cfg <: runtime.TransformerCfg: Type]
            : Type[runtime.TransformerCfg.FieldComputedPartial[Name, Cfg]] =
          weakTypeTag[runtime.TransformerCfg.FieldComputedPartial[Name, Cfg]]
        def unapply[A](A: Type[A]): Option[(?<[String], ?<[runtime.TransformerCfg])] =
          if (A.isCtor[runtime.TransformerCfg.FieldComputedPartial[?, ?]])
            Some(A.param_<[String](0) -> A.param_<[runtime.TransformerCfg](1))
          else scala.None
      }
      object FieldRelabelled extends FieldRelabelledModule {
        def apply[FromName <: String: Type, ToName <: String: Type, Cfg <: runtime.TransformerCfg: Type]
            : Type[runtime.TransformerCfg.FieldRelabelled[FromName, ToName, Cfg]] =
          weakTypeTag[runtime.TransformerCfg.FieldRelabelled[FromName, ToName, Cfg]]
        def unapply[A](A: Type[A]): Option[(?<[String], ?<[String], ?<[runtime.TransformerCfg])] =
          if (A.isCtor[runtime.TransformerCfg.FieldRelabelled[?, ?, ?]])
            Some((A.param_<[String](0), A.param_<[String](1), A.param_<[runtime.TransformerCfg](2)))
          else scala.None
      }
      object CoproductInstance extends CoproductInstanceModule {
        def apply[InstType: Type, TargetType: Type, Cfg <: runtime.TransformerCfg: Type]
            : Type[runtime.TransformerCfg.CoproductInstance[InstType, TargetType, Cfg]] =
          weakTypeTag[runtime.TransformerCfg.CoproductInstance[InstType, TargetType, Cfg]]
        def unapply[A](A: Type[A]): Option[(??, ??, ?<[runtime.TransformerCfg])] =
          if (A.isCtor[runtime.TransformerCfg.CoproductInstance[?, ?, ?]])
            Some((A.param(0), A.param(1), A.param_<[runtime.TransformerCfg](2)))
          else scala.None
      }
      object CoproductInstancePartial extends CoproductInstancePartialModule {
        def apply[InstType: Type, TargetType: Type, Cfg <: runtime.TransformerCfg: Type]
            : Type[runtime.TransformerCfg.CoproductInstancePartial[InstType, TargetType, Cfg]] =
          weakTypeTag[runtime.TransformerCfg.CoproductInstancePartial[InstType, TargetType, Cfg]]
        def unapply[A](A: Type[A]): Option[(??, ??, ?<[runtime.TransformerCfg])] =
          if (A.isCtor[runtime.TransformerCfg.CoproductInstancePartial[?, ?, ?]])
            Some((A.param(0), A.param(1), A.param_<[runtime.TransformerCfg](2)))
          else scala.None
      }
    }

    object TransformerFlags extends TransformerFlagsModule {
      val Default: Type[runtime.TransformerFlags.Default] = weakTypeTag[runtime.TransformerFlags.Default]

      object Enable extends EnableModule {
        def apply[F <: runtime.TransformerFlags.Flag: Type, Flags <: runtime.TransformerFlags: Type]
            : Type[runtime.TransformerFlags.Enable[F, Flags]] =
          weakTypeTag[runtime.TransformerFlags.Enable[F, Flags]]
        def unapply[A](A: Type[A]): Option[(?<[runtime.TransformerFlags.Flag], ?<[runtime.TransformerFlags])] =
          if (A.isCtor[runtime.TransformerFlags.Enable[?, ?]])
            Some(A.param_<[runtime.TransformerFlags.Flag](0) -> A.param_<[runtime.TransformerFlags](1))
          else scala.None
      }
      object Disable extends DisableModule {
        def apply[F <: runtime.TransformerFlags.Flag: Type, Flags <: runtime.TransformerFlags: Type]
            : Type[runtime.TransformerFlags.Disable[F, Flags]] =
          weakTypeTag[runtime.TransformerFlags.Disable[F, Flags]]
        def unapply[A](A: Type[A]): Option[(?<[runtime.TransformerFlags.Flag], ?<[runtime.TransformerFlags])] =
          if (A.isCtor[runtime.TransformerFlags.Disable[?, ?]])
            Some(A.param_<[runtime.TransformerFlags.Flag](0) -> A.param_<[runtime.TransformerFlags](1))
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
          def apply[R <: dsls.ImplicitTransformerPreference: Type]
              : Type[runtime.TransformerFlags.ImplicitConflictResolution[R]] =
            weakTypeTag[runtime.TransformerFlags.ImplicitConflictResolution[R]]
          def unapply[A](A: Type[A]): Option[?<[dsls.ImplicitTransformerPreference]] =
            if (A.isCtor[runtime.TransformerFlags.ImplicitConflictResolution[?]])
              Some(A.param_<[dsls.ImplicitTransformerPreference](0))
            else scala.None
        }
        val MacrosLogging: Type[runtime.TransformerFlags.MacrosLogging] =
          weakTypeTag[runtime.TransformerFlags.MacrosLogging]
      }
    }

    object PatcherCfg extends PatcherCfgModule {
      val Empty: Type[runtime.PatcherCfg.Empty] = weakTypeTag[runtime.PatcherCfg.Empty]

      object IgnoreRedundantPatcherFields extends IgnoreRedundantPatcherFieldsModule {
        def apply[Cfg <: runtime.PatcherCfg: Type]: Type[runtime.PatcherCfg.IgnoreRedundantPatcherFields[Cfg]] =
          weakTypeTag[runtime.PatcherCfg.IgnoreRedundantPatcherFields[Cfg]]
        def unapply[A](A: Type[A]): Option[?<[runtime.PatcherCfg]] =
          if (A.isCtor[runtime.PatcherCfg.IgnoreRedundantPatcherFields[?]]) Some(A.param_<[runtime.PatcherCfg](0))
          else scala.None
      }

      object IgnoreNoneInPatch extends IgnoreNoneInPatchModule {
        def apply[Cfg <: runtime.PatcherCfg: Type]: Type[runtime.PatcherCfg.IgnoreNoneInPatch[Cfg]] =
          weakTypeTag[runtime.PatcherCfg.IgnoreNoneInPatch[Cfg]]
        def unapply[A](A: Type[A]): Option[?<[runtime.PatcherCfg]] =
          if (A.isCtor[runtime.PatcherCfg.IgnoreNoneInPatch[?]]) Some(A.param_<[runtime.PatcherCfg](0))
          else scala.None
      }

      object MacrosLogging extends MacrosLoggingModule {
        def apply[Cfg <: runtime.PatcherCfg: Type]: Type[runtime.PatcherCfg.MacrosLogging[Cfg]] =
          weakTypeTag[runtime.PatcherCfg.MacrosLogging[Cfg]]
        def unapply[A](A: Type[A]): Option[?<[runtime.PatcherCfg]] =
          if (A.isCtor[runtime.PatcherCfg.MacrosLogging[?]]) Some(A.param_<[runtime.PatcherCfg](0))
          else scala.None
      }
    }

    object TransformerInto extends TransformerIntoModule {
      def apply[From: Type, To: Type, Cfg <: runtime.TransformerCfg: Type, Flags <: runtime.TransformerFlags: Type]
          : Type[dsls.TransformerInto[From, To, Cfg, Flags]] = weakTypeTag[dsls.TransformerInto[From, To, Cfg, Flags]]
      def unapply[A](A: Type[A]): Option[(??, ??, ?<[runtime.TransformerCfg], ?<[runtime.TransformerFlags])] =
        if (A.tpe.typeConstructor <:< weakTypeOf[dsls.TransformerInto[?, ?, ?, ?]].typeConstructor)
          Some((A.param(0), A.param(1), A.param_<[runtime.TransformerCfg](2), A.param_<[runtime.TransformerFlags](3)))
        else scala.None
    }

    object TransformerDefinition extends TransformerDefinitionModule {
      def apply[From: Type, To: Type, Cfg <: runtime.TransformerCfg: Type, Flags <: runtime.TransformerFlags: Type]
          : Type[dsls.TransformerDefinition[From, To, Cfg, Flags]] =
        weakTypeTag[dsls.TransformerDefinition[From, To, Cfg, Flags]]
      def unapply[A](A: Type[A]): Option[(??, ??, ?<[runtime.TransformerCfg], ?<[runtime.TransformerFlags])] =
        if (A.tpe.typeConstructor <:< weakTypeOf[dsls.TransformerDefinition[?, ?, ?, ?]].typeConstructor)
          Some((A.param(0), A.param(1), A.param_<[runtime.TransformerCfg](2), A.param_<[runtime.TransformerFlags](3)))
        else scala.None
    }

    object PartialTransformerInto extends PartialTransformerIntoModule {
      def apply[From: Type, To: Type, Cfg <: runtime.TransformerCfg: Type, Flags <: runtime.TransformerFlags: Type]
          : Type[dsls.PartialTransformerInto[From, To, Cfg, Flags]] =
        weakTypeTag[dsls.PartialTransformerInto[From, To, Cfg, Flags]]
      def unapply[A](A: Type[A]): Option[(??, ??, ?<[runtime.TransformerCfg], ?<[runtime.TransformerFlags])] =
        if (A.tpe.typeConstructor <:< weakTypeOf[dsls.PartialTransformerInto[?, ?, ?, ?]].typeConstructor)
          Some((A.param(0), A.param(1), A.param_<[runtime.TransformerCfg](2), A.param_<[runtime.TransformerFlags](3)))
        else scala.None
    }

    object PartialTransformerDefinition extends PartialTransformerDefinitionModule {
      def apply[From: Type, To: Type, Cfg <: runtime.TransformerCfg: Type, Flags <: runtime.TransformerFlags: Type]
          : Type[dsls.PartialTransformerDefinition[From, To, Cfg, Flags]] =
        weakTypeTag[dsls.PartialTransformerDefinition[From, To, Cfg, Flags]]

      def unapply[A](A: Type[A]): Option[(??, ??, ?<[runtime.TransformerCfg], ?<[runtime.TransformerFlags])] =
        if (A.tpe.typeConstructor <:< weakTypeOf[dsls.PartialTransformerDefinition[?, ?, ?, ?]].typeConstructor)
          Some((A.param(0), A.param(1), A.param_<[runtime.TransformerCfg](2), A.param_<[runtime.TransformerFlags](3)))
        else scala.None
    }

    val RuntimeDataStore: Type[dsls.TransformerDefinitionCommons.RuntimeDataStore] =
      weakTypeTag[dsls.TransformerDefinitionCommons.RuntimeDataStore]
  }
}
