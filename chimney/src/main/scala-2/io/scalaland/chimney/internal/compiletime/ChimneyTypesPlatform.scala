package io.scalaland.chimney.internal.compiletime

import io.scalaland.chimney.{PartialTransformer, Patcher, Transformer}
import io.scalaland.chimney.dsl as dsls
import io.scalaland.chimney.internal.runtime
import io.scalaland.chimney.partial

private[compiletime] trait ChimneyTypesPlatform extends ChimneyTypes { this: ChimneyDefinitionsPlatform =>

  import c.universe.{internal as _, Name as _, Transformer as _, *}

  protected object ChimneyType extends ChimneyTypeModule {

    import Type.platformSpecific.*

    object platformSpecific {

      /** Workaround for Java Enums, see [[io.scalaland.chimney.internal.runtime.RefinedJavaEnum]]. */
      def fixJavaEnums(inst: ??): ?? =
        if (inst.Underlying.isCtor[runtime.RefinedJavaEnum[?, ?]]) {
          val javaEnum = inst.Underlying.param(0).Underlying.tpe
          val instance = inst.Underlying.param(1).Underlying.asInstanceOf[Type[String]].extractStringSingleton

          javaEnum.companion.decls
            .filter(_.isJavaEnum)
            .collectFirst {
              case sym if sym.name.decodedName.toString == instance => fromUntyped[Any](sym.asTerm.typeSignature).as_??
            }
            .getOrElse {
              reportError("Failed at encoding Java Enum instance type")
            }
        } else inst
    }
    import platformSpecific.fixJavaEnums

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

    object ArgumentList extends ArgumentListModule {
      val Empty: Type[runtime.ArgumentList.Empty] = weakTypeTag[runtime.ArgumentList.Empty]
      object Argument extends ArgumentModule {
        def apply[Name <: String: Type, Tpe: Type, Args <: runtime.ArgumentList: Type]
            : Type[runtime.ArgumentList.Argument[Name, Tpe, Args]] =
          weakTypeTag[runtime.ArgumentList.Argument[Name, Tpe, Args]]
        def unapply[A](A: Type[A]): Option[(?<[String], ??, ?<[runtime.ArgumentList])] =
          if (A.isCtor[runtime.ArgumentList.Argument[?, ?, ?]])
            Some((A.param_<[String](0), A.param(1), A.param_<[runtime.ArgumentList](2)))
          else scala.None
      }
    }

    object ArgumentLists extends ArgumentListsModule {
      val Empty: Type[runtime.ArgumentLists.Empty] = weakTypeTag[runtime.ArgumentLists.Empty]
      object List extends ListModule {
        def apply[Head <: runtime.ArgumentList: Type, Tail <: runtime.ArgumentLists: Type]
            : Type[runtime.ArgumentLists.List[Head, Tail]] =
          weakTypeTag[runtime.ArgumentLists.List[Head, Tail]]
        def unapply[A](A: Type[A]): Option[(?<[runtime.ArgumentList], ?<[runtime.ArgumentLists])] =
          if (A.isCtor[runtime.ArgumentLists.List[?, ?]])
            Some((A.param_<[runtime.ArgumentList](0), A.param_<[runtime.ArgumentLists](1)))
          else scala.None
      }
    }

    object TransformerCfg extends TransformerCfgModule {
      val Empty: Type[runtime.TransformerCfg.Empty] = weakTypeTag[runtime.TransformerCfg.Empty]
      object FieldConst extends FieldConstModule {
        def apply[Name <: runtime.Path: Type, Cfg <: runtime.TransformerCfg: Type]
            : Type[runtime.TransformerCfg.FieldConst[Name, Cfg]] =
          weakTypeTag[runtime.TransformerCfg.FieldConst[Name, Cfg]]
        def unapply[A](A: Type[A]): Option[(?<[runtime.Path], ?<[runtime.TransformerCfg])] =
          if (A.isCtor[runtime.TransformerCfg.FieldConst[?, ?]])
            Some(A.param_<[runtime.Path](0) -> A.param_<[runtime.TransformerCfg](1))
          else scala.None
      }
      object FieldConstPartial extends FieldConstPartialModule {
        def apply[Name <: runtime.Path: Type, Cfg <: runtime.TransformerCfg: Type]
            : Type[runtime.TransformerCfg.FieldConstPartial[Name, Cfg]] =
          weakTypeTag[runtime.TransformerCfg.FieldConstPartial[Name, Cfg]]
        def unapply[A](A: Type[A]): Option[(?<[runtime.Path], ?<[runtime.TransformerCfg])] =
          if (A.isCtor[runtime.TransformerCfg.FieldConstPartial[?, ?]])
            Some(A.param_<[runtime.Path](0) -> A.param_<[runtime.TransformerCfg](1))
          else scala.None
      }
      object FieldComputed extends FieldComputedModule {
        def apply[Name <: runtime.Path: Type, Cfg <: runtime.TransformerCfg: Type]
            : Type[runtime.TransformerCfg.FieldComputed[Name, Cfg]] =
          weakTypeTag[runtime.TransformerCfg.FieldComputed[Name, Cfg]]
        def unapply[A](A: Type[A]): Option[(?<[runtime.Path], ?<[runtime.TransformerCfg])] =
          if (A.isCtor[runtime.TransformerCfg.FieldComputed[?, ?]])
            Some(A.param_<[runtime.Path](0) -> A.param_<[runtime.TransformerCfg](1))
          else scala.None
      }
      object FieldComputedPartial extends FieldComputedPartialModule {
        def apply[Name <: runtime.Path: Type, Cfg <: runtime.TransformerCfg: Type]
            : Type[runtime.TransformerCfg.FieldComputedPartial[Name, Cfg]] =
          weakTypeTag[runtime.TransformerCfg.FieldComputedPartial[Name, Cfg]]
        def unapply[A](A: Type[A]): Option[(?<[runtime.Path], ?<[runtime.TransformerCfg])] =
          if (A.isCtor[runtime.TransformerCfg.FieldComputedPartial[?, ?]])
            Some(A.param_<[runtime.Path](0) -> A.param_<[runtime.TransformerCfg](1))
          else scala.None
      }
      object FieldRelabelled extends FieldRelabelledModule {
        def apply[FromName <: runtime.Path: Type, ToName <: runtime.Path: Type, Cfg <: runtime.TransformerCfg: Type]
            : Type[runtime.TransformerCfg.FieldRelabelled[FromName, ToName, Cfg]] =
          weakTypeTag[runtime.TransformerCfg.FieldRelabelled[FromName, ToName, Cfg]]
        def unapply[A](A: Type[A]): Option[(?<[runtime.Path], ?<[runtime.Path], ?<[runtime.TransformerCfg])] =
          if (A.isCtor[runtime.TransformerCfg.FieldRelabelled[?, ?, ?]])
            Some((A.param_<[runtime.Path](0), A.param_<[runtime.Path](1), A.param_<[runtime.TransformerCfg](2)))
          else scala.None
      }
      object CoproductInstance extends CoproductInstanceModule {
        def apply[InstType: Type, TargetType: Type, Cfg <: runtime.TransformerCfg: Type]
            : Type[runtime.TransformerCfg.CoproductInstance[InstType, TargetType, Cfg]] =
          weakTypeTag[runtime.TransformerCfg.CoproductInstance[InstType, TargetType, Cfg]]
        def unapply[A](A: Type[A]): Option[(??, ??, ?<[runtime.TransformerCfg])] =
          if (A.isCtor[runtime.TransformerCfg.CoproductInstance[?, ?, ?]])
            Some((fixJavaEnums(A.param(0)), A.param(1), A.param_<[runtime.TransformerCfg](2)))
          else scala.None
      }
      object CoproductInstancePartial extends CoproductInstancePartialModule {
        def apply[InstType: Type, TargetType: Type, Cfg <: runtime.TransformerCfg: Type]
            : Type[runtime.TransformerCfg.CoproductInstancePartial[InstType, TargetType, Cfg]] =
          weakTypeTag[runtime.TransformerCfg.CoproductInstancePartial[InstType, TargetType, Cfg]]
        def unapply[A](A: Type[A]): Option[(??, ??, ?<[runtime.TransformerCfg])] =
          if (A.isCtor[runtime.TransformerCfg.CoproductInstancePartial[?, ?, ?]])
            Some((fixJavaEnums(A.param(0)), A.param(1), A.param_<[runtime.TransformerCfg](2)))
          else scala.None
      }
      object Constructor extends ConstructorModule {
        def apply[Args <: runtime.ArgumentLists: Type, TargetType: Type, Cfg <: runtime.TransformerCfg: Type]
            : Type[runtime.TransformerCfg.Constructor[Args, TargetType, Cfg]] =
          weakTypeTag[runtime.TransformerCfg.Constructor[Args, TargetType, Cfg]]
        def unapply[A](A: Type[A]): Option[(?<[runtime.ArgumentLists], ??, ?<[runtime.TransformerCfg])] =
          if (A.isCtor[runtime.TransformerCfg.Constructor[?, ?, ?]])
            Some((A.param_<[runtime.ArgumentLists](0), A.param(1), A.param_<[runtime.TransformerCfg](2)))
          else scala.None
      }
      object ConstructorPartial extends ConstructorPartialModule {
        def apply[Args <: runtime.ArgumentLists: Type, TargetType: Type, Cfg <: runtime.TransformerCfg: Type]
            : Type[runtime.TransformerCfg.ConstructorPartial[Args, TargetType, Cfg]] =
          weakTypeTag[runtime.TransformerCfg.ConstructorPartial[Args, TargetType, Cfg]]
        def unapply[A](A: Type[A]): Option[(?<[runtime.ArgumentLists], ??, ?<[runtime.TransformerCfg])] =
          if (A.isCtor[runtime.TransformerCfg.ConstructorPartial[?, ?, ?]])
            Some((A.param_<[runtime.ArgumentLists](0), A.param(1), A.param_<[runtime.TransformerCfg](2)))
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
        val InheritedAccessors: Type[runtime.TransformerFlags.InheritedAccessors] =
          weakTypeTag[runtime.TransformerFlags.InheritedAccessors]
        val MethodAccessors: Type[runtime.TransformerFlags.MethodAccessors] =
          weakTypeTag[runtime.TransformerFlags.MethodAccessors]
        val DefaultValues: Type[runtime.TransformerFlags.DefaultValues] =
          weakTypeTag[runtime.TransformerFlags.DefaultValues]
        val BeanGetters: Type[runtime.TransformerFlags.BeanGetters] =
          weakTypeTag[runtime.TransformerFlags.BeanGetters]
        val BeanSetters: Type[runtime.TransformerFlags.BeanSetters] =
          weakTypeTag[runtime.TransformerFlags.BeanSetters]
        val BeanSettersIgnoreUnmatched: Type[runtime.TransformerFlags.BeanSettersIgnoreUnmatched] =
          weakTypeTag[runtime.TransformerFlags.BeanSettersIgnoreUnmatched]
        val NonUnitBeanSetters: Type[runtime.TransformerFlags.NonUnitBeanSetters] =
          weakTypeTag[runtime.TransformerFlags.NonUnitBeanSetters]
        val OptionDefaultsToNone: Type[runtime.TransformerFlags.OptionDefaultsToNone] =
          weakTypeTag[runtime.TransformerFlags.OptionDefaultsToNone]
        val PartialUnwrapsOption: Type[runtime.TransformerFlags.PartialUnwrapsOption] =
          weakTypeTag[runtime.TransformerFlags.PartialUnwrapsOption]
        object ImplicitConflictResolution extends ImplicitConflictResolutionModule {
          def apply[R <: dsls.ImplicitTransformerPreference: Type]
              : Type[runtime.TransformerFlags.ImplicitConflictResolution[R]] =
            weakTypeTag[runtime.TransformerFlags.ImplicitConflictResolution[R]]
          def unapply[A](A: Type[A]): Option[?<[dsls.ImplicitTransformerPreference]] =
            if (A.isCtor[runtime.TransformerFlags.ImplicitConflictResolution[?]])
              Some(A.param_<[dsls.ImplicitTransformerPreference](0))
            else scala.None
        }
        object FieldNameComparison extends FieldNameComparisonModule {
          def apply[C <: dsls.TransformedNamesComparison: Type]: Type[runtime.TransformerFlags.FieldNameComparison[C]] =
            weakTypeTag[runtime.TransformerFlags.FieldNameComparison[C]]
          def unapply[A](A: Type[A]): Option[?<[dsls.TransformedNamesComparison]] =
            if (A.isCtor[runtime.TransformerFlags.FieldNameComparison[?]])
              Some(A.param_<[dsls.TransformedNamesComparison](0))
            else scala.None
        }
        object SubtypeNameComparison extends SubtypeNameComparisonModule {
          def apply[C <: dsls.TransformedNamesComparison: Type]
              : Type[runtime.TransformerFlags.SubtypeNameComparison[C]] =
            weakTypeTag[runtime.TransformerFlags.SubtypeNameComparison[C]]
          def unapply[A](A: Type[A]): Option[?<[dsls.TransformedNamesComparison]] =
            if (A.isCtor[runtime.TransformerFlags.SubtypeNameComparison[?]])
              Some(A.param_<[dsls.TransformedNamesComparison](0))
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

    object Path extends PathModule {
      val Root: Type[runtime.Path.Root] = weakTypeTag[runtime.Path.Root]
      object Select extends SelectModule {
        def apply[Name <: String: Type, Instance <: runtime.Path: Type]: Type[runtime.Path.Select[Name, Instance]] =
          weakTypeTag[runtime.Path.Select[Name, Instance]]
        def unapply[A](A: Type[A]): Option[(?<[String], ?<[runtime.Path])] =
          if (A.isCtor[runtime.Path.Select[?, ?]]) Some(A.param_<[String](0) -> A.param_<[runtime.Path](1))
          else scala.None
      }
    }
  }
}
