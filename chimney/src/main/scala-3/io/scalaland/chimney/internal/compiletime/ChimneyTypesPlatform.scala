package io.scalaland.chimney.internal.compiletime

import io.scalaland.chimney.{PartialTransformer, Patcher, Transformer}
import io.scalaland.chimney.dsl as dsls
import io.scalaland.chimney.internal.runtime
import io.scalaland.chimney.partial

import scala.quoted

private[compiletime] trait ChimneyTypesPlatform extends ChimneyTypes { this: ChimneyDefinitionsPlatform =>

  import quotes.*, quotes.reflect.*

  object ChimneyType extends ChimneyTypeModule {

    def Transformer[From: Type, To: Type]: Type[Transformer[From, To]] = quoted.Type.of[Transformer[From, To]]
    def PartialTransformer[From: Type, To: Type]: Type[PartialTransformer[From, To]] =
      quoted.Type.of[PartialTransformer[From, To]]
    def Patcher[T: Type, Patch: Type]: Type[Patcher[T, Patch]] = quoted.Type.of[Patcher[T, Patch]]

    object PartialResult extends PartialResultModule {
      def apply[T: Type]: Type[partial.Result[T]] = quoted.Type.of[partial.Result[T]]
      def unapply[T](tpe: Type[T]): Option[??] = tpe match
        case '[partial.Result[inner]] => Some(Type[inner].as_??)
        case _                        => scala.None

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

    val RuntimeDataStore: Type[dsls.TransformerDefinitionCommons.RuntimeDataStore] =
      quoted.Type.of[dsls.TransformerDefinitionCommons.RuntimeDataStore]

    object TransformerCfg extends TransformerCfgModule {
      val Empty: Type[runtime.TransformerCfg.Empty] = quoted.Type.of[runtime.TransformerCfg.Empty]
      object FieldConst extends FieldConstModule {
        def apply[Name <: String: Type, Cfg <: runtime.TransformerCfg: Type]
            : Type[runtime.TransformerCfg.FieldConst[Name, Cfg]] =
          quoted.Type.of[runtime.TransformerCfg.FieldConst[Name, Cfg]]
        def unapply[A](tpe: Type[A]): Option[(?<[String], ?<[runtime.TransformerCfg])] = tpe match
          case '[runtime.TransformerCfg.FieldConst[name, c]] =>
            Some((Type[name].as_?<[String], Type[c].as_?<[runtime.TransformerCfg]))
          case _ => scala.None
      }
      object FieldConstPartial extends FieldConstPartialModule {
        def apply[Name <: String: Type, Cfg <: runtime.TransformerCfg: Type]
            : Type[runtime.TransformerCfg.FieldConstPartial[Name, Cfg]] =
          quoted.Type.of[runtime.TransformerCfg.FieldConstPartial[Name, Cfg]]
        def unapply[A](tpe: Type[A]): Option[(?<[String], ?<[runtime.TransformerCfg])] = tpe match
          case '[runtime.TransformerCfg.FieldConstPartial[name, c]] =>
            Some((Type[name].as_>?<[Nothing, String], Type[c].as_>?<[Nothing, runtime.TransformerCfg]))
          case _ => scala.None
      }
      object FieldComputed extends FieldComputedModule {
        def apply[Name <: String: Type, Cfg <: runtime.TransformerCfg: Type]
            : Type[runtime.TransformerCfg.FieldComputed[Name, Cfg]] =
          quoted.Type.of[runtime.TransformerCfg.FieldComputed[Name, Cfg]]
        def unapply[A](tpe: Type[A]): Option[(?<[String], ?<[runtime.TransformerCfg])] = tpe match
          case '[runtime.TransformerCfg.FieldComputed[name, c]] =>
            Some((Type[name].as_?<[String], Type[c].as_?<[runtime.TransformerCfg]))
          case _ => scala.None
      }
      object FieldComputedPartial extends FieldComputedPartialModule {
        def apply[Name <: String: Type, Cfg <: runtime.TransformerCfg: Type]
            : Type[runtime.TransformerCfg.FieldComputedPartial[Name, Cfg]] =
          quoted.Type.of[runtime.TransformerCfg.FieldComputedPartial[Name, Cfg]]
        def unapply[A](tpe: Type[A]): Option[(Nothing >?< String, Nothing >?< runtime.TransformerCfg)] = tpe match
          case '[runtime.TransformerCfg.FieldComputedPartial[name, c]] =>
            Some((Type[name].as_?<[String], Type[c].as_?<[runtime.TransformerCfg]))
          case _ => scala.None
      }
      object FieldRelabelled extends FieldRelabelledModule {
        def apply[FromName <: String: Type, ToName <: String: Type, Cfg <: runtime.TransformerCfg: Type]
            : Type[runtime.TransformerCfg.FieldRelabelled[FromName, ToName, Cfg]] =
          quoted.Type.of[runtime.TransformerCfg.FieldRelabelled[FromName, ToName, Cfg]]
        def unapply[A](tpe: Type[A]): Option[(?<[String], ?<[String], ?<[runtime.TransformerCfg])] = tpe match
          case '[runtime.TransformerCfg.FieldRelabelled[fromName, toName, c]] =>
            Some((Type[fromName].as_?<[String], Type[toName].as_?<[String], Type[c].as_?<[runtime.TransformerCfg]))
          case _ => scala.None
      }
      object CoproductInstance extends CoproductInstanceModule {
        def apply[InstType: Type, TargetType: Type, Cfg <: runtime.TransformerCfg: Type]
            : Type[runtime.TransformerCfg.CoproductInstance[InstType, TargetType, Cfg]] =
          quoted.Type.of[runtime.TransformerCfg.CoproductInstance[InstType, TargetType, Cfg]]
        def unapply[A](tpe: Type[A]): Option[(??, ??, ?<[runtime.TransformerCfg])] = tpe match
          case '[runtime.TransformerCfg.CoproductInstance[instType, targetType, c]] =>
            Some((Type[instType].as_??, Type[targetType].as_??, Type[c].as_?<[runtime.TransformerCfg]))
          case _ => scala.None
      }
      object CoproductInstancePartial extends CoproductInstancePartialModule {
        def apply[InstType: Type, TargetType: Type, Cfg <: runtime.TransformerCfg: Type]
            : Type[runtime.TransformerCfg.CoproductInstancePartial[InstType, TargetType, Cfg]] =
          quoted.Type.of[runtime.TransformerCfg.CoproductInstancePartial[InstType, TargetType, Cfg]]
        def unapply[A](tpe: Type[A]): Option[(??, ??, ?<[runtime.TransformerCfg])] = tpe match
          case '[runtime.TransformerCfg.CoproductInstancePartial[instType, targetType, c]] =>
            Some((Type[instType].as_??, Type[targetType].as_??, Type[c].as_?<[runtime.TransformerCfg]))
          case _ => scala.None
      }
    }

    object TransformerFlags extends TransformerFlagsModule {
      val Default: Type[runtime.TransformerFlags.Default] = quoted.Type.of[runtime.TransformerFlags.Default]

      object Enable extends EnableModule {
        def apply[F <: runtime.TransformerFlags.Flag: Type, Flags <: runtime.TransformerFlags: Type]
            : Type[runtime.TransformerFlags.Enable[F, Flags]] =
          quoted.Type.of[runtime.TransformerFlags.Enable[F, Flags]]
        def unapply[A](tpe: Type[A]): Option[(?<[runtime.TransformerFlags.Flag], ?<[runtime.TransformerFlags])] =
          tpe match
            case '[runtime.TransformerFlags.Enable[f, flags]] =>
              Some((Type[f].as_?<[runtime.TransformerFlags.Flag], Type[flags].as_?<[runtime.TransformerFlags]))
            case _ => scala.None
      }
      object Disable extends DisableModule {
        def apply[F <: runtime.TransformerFlags.Flag: Type, Flags <: runtime.TransformerFlags: Type]
            : Type[runtime.TransformerFlags.Disable[F, Flags]] =
          quoted.Type.of[runtime.TransformerFlags.Disable[F, Flags]]
        def unapply[A](tpe: Type[A]): Option[(?<[runtime.TransformerFlags.Flag], ?<[runtime.TransformerFlags])] =
          tpe match
            case '[runtime.TransformerFlags.Disable[f, flags]] =>
              Some((Type[f].as_?<[runtime.TransformerFlags.Flag], Type[flags].as_?<[runtime.TransformerFlags]))
            case _ => scala.None
      }

      object Flags extends FlagsModule {
        val DefaultValues: Type[runtime.TransformerFlags.DefaultValues] =
          quoted.Type.of[runtime.TransformerFlags.DefaultValues]
        val BeanGetters: Type[runtime.TransformerFlags.BeanGetters] =
          quoted.Type.of[runtime.TransformerFlags.BeanGetters]
        val BeanSetters: Type[runtime.TransformerFlags.BeanSetters] =
          quoted.Type.of[runtime.TransformerFlags.BeanSetters]
        val MethodAccessors: Type[runtime.TransformerFlags.MethodAccessors] =
          quoted.Type.of[runtime.TransformerFlags.MethodAccessors]
        val OptionDefaultsToNone: Type[runtime.TransformerFlags.OptionDefaultsToNone] =
          quoted.Type.of[runtime.TransformerFlags.OptionDefaultsToNone]
        object ImplicitConflictResolution extends ImplicitConflictResolutionModule {
          def apply[R <: dsls.ImplicitTransformerPreference: Type]
              : Type[runtime.TransformerFlags.ImplicitConflictResolution[R]] =
            quoted.Type.of[runtime.TransformerFlags.ImplicitConflictResolution[R]]
          def unapply[A](tpe: Type[A]): Option[?<[dsls.ImplicitTransformerPreference]] = tpe match
            case '[runtime.TransformerFlags.ImplicitConflictResolution[r]] =>
              Some(Type[r].as_?<[dsls.ImplicitTransformerPreference])
            case _ => scala.None
        }
        val MacrosLogging: Type[runtime.TransformerFlags.MacrosLogging] =
          quoted.Type.of[runtime.TransformerFlags.MacrosLogging]
      }
    }

    object PatcherCfg extends PatcherCfgModule {
      val Empty: Type[runtime.PatcherCfg.Empty] = quoted.Type.of[runtime.PatcherCfg.Empty]
    }

    object PatcherFlags extends PatcherFlagsModule {
      val Default: Type[runtime.PatcherFlags.Default] = quoted.Type.of[runtime.PatcherFlags.Default]

      object Enable extends EnableModule {
        def apply[F <: runtime.PatcherFlags.Flag: Type, Flags <: runtime.PatcherFlags: Type]
            : Type[runtime.PatcherFlags.Enable[F, Flags]] =
          quoted.Type.of[runtime.PatcherFlags.Enable[F, Flags]]

        def unapply[A](tpe: Type[A]): Option[(?<[runtime.PatcherFlags.Flag], ?<[runtime.PatcherFlags])] =
          tpe match
            case '[runtime.PatcherFlags.Enable[f, flags]] =>
              Some((Type[f].as_?<[runtime.PatcherFlags.Flag], Type[flags].as_?<[runtime.PatcherFlags]))
            case _ => scala.None
      }
      object Disable extends DisableModule {
        def apply[F <: runtime.PatcherFlags.Flag: Type, Flags <: runtime.PatcherFlags: Type]
            : Type[runtime.PatcherFlags.Disable[F, Flags]] =
          quoted.Type.of[runtime.PatcherFlags.Disable[F, Flags]]

        def unapply[A](tpe: Type[A]): Option[(?<[runtime.PatcherFlags.Flag], ?<[runtime.PatcherFlags])] =
          tpe match
            case '[runtime.PatcherFlags.Disable[f, flags]] =>
              Some((Type[f].as_?<[runtime.PatcherFlags.Flag], Type[flags].as_?<[runtime.PatcherFlags]))
            case _ => scala.None
      }

      object Flags extends FlagsModule {
        val IgnoreNoneInPatch: Type[runtime.PatcherFlags.IgnoreNoneInPatch] =
          quoted.Type.of[runtime.PatcherFlags.IgnoreNoneInPatch]
        val IgnoreRedundantPatcherFields: Type[runtime.PatcherFlags.IgnoreRedundantPatcherFields] =
          quoted.Type.of[runtime.PatcherFlags.IgnoreRedundantPatcherFields]
        val MacrosLogging: Type[runtime.PatcherFlags.MacrosLogging] = quoted.Type.of[runtime.PatcherFlags.MacrosLogging]
      }
    }
  }
}
