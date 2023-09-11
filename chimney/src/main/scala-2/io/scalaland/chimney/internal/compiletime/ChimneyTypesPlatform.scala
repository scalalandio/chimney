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

    val RuntimeDataStore: Type[dsls.TransformerDefinitionCommons.RuntimeDataStore] =
      weakTypeTag[dsls.TransformerDefinitionCommons.RuntimeDataStore]

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
    }

    object PatcherFlags extends PatcherFlagsModule {
      val Default: Type[runtime.PatcherFlags.Default] = weakTypeTag[runtime.PatcherFlags.Default]

      object Enable extends EnableModule {
        def apply[F <: runtime.PatcherFlags.Flag: Type, Flags <: runtime.PatcherFlags: Type]
            : Type[runtime.PatcherFlags.Enable[F, Flags]] =
          weakTypeTag[runtime.PatcherFlags.Enable[F, Flags]]

        def unapply[A](A: Type[A]): Option[(?<[runtime.PatcherFlags.Flag], ?<[runtime.PatcherFlags])] =
          if (A.isCtor[runtime.PatcherFlags.Enable[?, ?]])
            Some(A.param_<[runtime.PatcherFlags.Flag](0) -> A.param_<[runtime.PatcherFlags](1))
          else scala.None
      }
      object Disable extends DisableModule {
        def apply[F <: runtime.PatcherFlags.Flag: Type, Flags <: runtime.PatcherFlags: Type]
            : Type[runtime.PatcherFlags.Disable[F, Flags]] =
          weakTypeTag[runtime.PatcherFlags.Disable[F, Flags]]

        def unapply[A](A: Type[A]): Option[(?<[runtime.PatcherFlags.Flag], ?<[runtime.PatcherFlags])] =
          if (A.isCtor[runtime.PatcherFlags.Disable[?, ?]])
            Some(A.param_<[runtime.PatcherFlags.Flag](0) -> A.param_<[runtime.PatcherFlags](1))
          else scala.None
      }

      object Flags extends FlagsModule {
        val IgnoreNoneInPatch: Type[runtime.PatcherFlags.IgnoreNoneInPatch] =
          weakTypeTag[runtime.PatcherFlags.IgnoreNoneInPatch]
        val IgnoreRedundantPatcherFields: Type[runtime.PatcherFlags.IgnoreRedundantPatcherFields] =
          weakTypeTag[runtime.PatcherFlags.IgnoreRedundantPatcherFields]
        val MacrosLogging: Type[runtime.PatcherFlags.MacrosLogging] = weakTypeTag[runtime.PatcherFlags.MacrosLogging]
      }
    }
  }
}
