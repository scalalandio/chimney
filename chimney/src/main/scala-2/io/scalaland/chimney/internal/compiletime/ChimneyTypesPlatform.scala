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
      def fixJavaEnum(inst: ??): ?? =
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

      def fixJavaEnums(path: ?<[runtime.Path]): ?<[runtime.Path] =
        path.Underlying match {
          case root if root =:= Path.Root => path
          case Path.Select(init, name) =>
            val fixedInit = fixJavaEnums(init)
            import name.Underlying as Name, fixedInit.Underlying as FixedInit
            Path.Select[FixedInit, Name].as_?<[runtime.Path]
          case Path.Matching(init, subtype) =>
            val fixedSubtype = fixJavaEnum(subtype)
            val fixedInit = fixJavaEnums(init)
            import fixedSubtype.Underlying as FixedSubtype, fixedInit.Underlying as FixedInit
            Path.Matching[FixedInit, FixedSubtype].as_?<[runtime.Path]
          case Path.SourceMatching(init, subtype) =>
            val fixedSubtype = fixJavaEnum(subtype)
            val fixedInit = fixJavaEnums(init)
            import fixedSubtype.Underlying as FixedSubtype, fixedInit.Underlying as FixedInit
            Path.SourceMatching[FixedInit, FixedSubtype].as_?<[runtime.Path]
          case Path.EveryItem(init) =>
            val fixedInit = fixJavaEnums(init)
            import fixedInit.Underlying as FixedInit
            Path.EveryItem[FixedInit].as_?<[runtime.Path]
          case Path.EveryMapKey(init) =>
            val fixedInit = fixJavaEnums(init)
            import fixedInit.Underlying as FixedInit
            Path.EveryMapKey[FixedInit].as_?<[runtime.Path]
          case Path.EveryMapValue(init) =>
            val fixedInit = fixJavaEnums(init)
            import fixedInit.Underlying as FixedInit
            Path.EveryMapValue[FixedInit].as_?<[runtime.Path]
          case _ => reportError(s"Expected valid runtime.Path, got ${Type.prettyPrint(path.Underlying)}")
        }
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

    object TransformerOverrides extends TransformerOverridesModule {
      val Empty: Type[runtime.TransformerOverrides.Empty] = weakTypeTag[runtime.TransformerOverrides.Empty]
      object Const extends ConstModule {
        def apply[ToPath <: runtime.Path: Type, Tail <: runtime.TransformerOverrides: Type]
            : Type[runtime.TransformerOverrides.Const[ToPath, Tail]] =
          weakTypeTag[runtime.TransformerOverrides.Const[ToPath, Tail]]
        def unapply[A](A: Type[A]): Option[(?<[runtime.Path], ?<[runtime.TransformerOverrides])] =
          if (A.isCtor[runtime.TransformerOverrides.Const[?, ?]])
            Some(fixJavaEnums(A.param_<[runtime.Path](0)) -> A.param_<[runtime.TransformerOverrides](1))
          else scala.None
      }
      object ConstPartial extends ConstPartialModule {
        def apply[ToPath <: runtime.Path: Type, Tail <: runtime.TransformerOverrides: Type]
            : Type[runtime.TransformerOverrides.ConstPartial[ToPath, Tail]] =
          weakTypeTag[runtime.TransformerOverrides.ConstPartial[ToPath, Tail]]
        def unapply[A](A: Type[A]): Option[(?<[runtime.Path], ?<[runtime.TransformerOverrides])] =
          if (A.isCtor[runtime.TransformerOverrides.ConstPartial[?, ?]])
            Some(fixJavaEnums(A.param_<[runtime.Path](0)) -> A.param_<[runtime.TransformerOverrides](1))
          else scala.None
      }
      object Computed extends ComputedModule {
        def apply[ToPath <: runtime.Path: Type, Tail <: runtime.TransformerOverrides: Type]
            : Type[runtime.TransformerOverrides.Computed[ToPath, Tail]] =
          weakTypeTag[runtime.TransformerOverrides.Computed[ToPath, Tail]]
        def unapply[A](A: Type[A]): Option[(?<[runtime.Path], ?<[runtime.TransformerOverrides])] =
          if (A.isCtor[runtime.TransformerOverrides.Computed[?, ?]])
            Some(fixJavaEnums(A.param_<[runtime.Path](0)) -> A.param_<[runtime.TransformerOverrides](1))
          else scala.None
      }
      object ComputedPartial extends ComputedPartialModule {
        def apply[ToPath <: runtime.Path: Type, Tail <: runtime.TransformerOverrides: Type]
            : Type[runtime.TransformerOverrides.ComputedPartial[ToPath, Tail]] =
          weakTypeTag[runtime.TransformerOverrides.ComputedPartial[ToPath, Tail]]
        def unapply[A](A: Type[A]): Option[(?<[runtime.Path], ?<[runtime.TransformerOverrides])] =
          if (A.isCtor[runtime.TransformerOverrides.ComputedPartial[?, ?]])
            Some(fixJavaEnums(A.param_<[runtime.Path](0)) -> A.param_<[runtime.TransformerOverrides](1))
          else scala.None
      }
      object CaseComputed extends CaseComputedModule {
        def apply[ToPath <: runtime.Path: Type, Tail <: runtime.TransformerOverrides: Type]
            : Type[runtime.TransformerOverrides.CaseComputed[ToPath, Tail]] =
          weakTypeTag[runtime.TransformerOverrides.CaseComputed[ToPath, Tail]]
        def unapply[A](A: Type[A]): Option[(?<[runtime.Path], ?<[runtime.TransformerOverrides])] =
          if (A.isCtor[runtime.TransformerOverrides.CaseComputed[?, ?]])
            Some(fixJavaEnums(A.param_<[runtime.Path](0)) -> A.param_<[runtime.TransformerOverrides](1))
          else scala.None
      }
      object CaseComputedPartial extends CaseComputedPartialModule {
        def apply[ToPath <: runtime.Path: Type, Tail <: runtime.TransformerOverrides: Type]
            : Type[runtime.TransformerOverrides.CaseComputedPartial[ToPath, Tail]] =
          weakTypeTag[runtime.TransformerOverrides.CaseComputedPartial[ToPath, Tail]]
        def unapply[A](A: Type[A]): Option[(?<[runtime.Path], ?<[runtime.TransformerOverrides])] =
          if (A.isCtor[runtime.TransformerOverrides.CaseComputedPartial[?, ?]])
            Some(fixJavaEnums(A.param_<[runtime.Path](0)) -> A.param_<[runtime.TransformerOverrides](1))
          else scala.None
      }
      object Constructor extends ConstructorModule {
        def apply[
            Args <: runtime.ArgumentLists: Type,
            ToPath <: runtime.Path: Type,
            Tail <: runtime.TransformerOverrides: Type
        ]: Type[runtime.TransformerOverrides.Constructor[Args, ToPath, Tail]] =
          weakTypeTag[runtime.TransformerOverrides.Constructor[Args, ToPath, Tail]]
        def unapply[A](
            A: Type[A]
        ): Option[(?<[runtime.ArgumentLists], ?<[runtime.Path], ?<[runtime.TransformerOverrides])] =
          if (A.isCtor[runtime.TransformerOverrides.Constructor[?, ?, ?]])
            Some(
              (
                A.param_<[runtime.ArgumentLists](0),
                fixJavaEnums(A.param_<[runtime.Path](1)),
                A.param_<[runtime.TransformerOverrides](2)
              )
            )
          else scala.None
      }
      object ConstructorPartial extends ConstructorPartialModule {
        def apply[
            Args <: runtime.ArgumentLists: Type,
            ToPath <: runtime.Path: Type,
            Tail <: runtime.TransformerOverrides: Type
        ]: Type[runtime.TransformerOverrides.ConstructorPartial[Args, ToPath, Tail]] =
          weakTypeTag[runtime.TransformerOverrides.ConstructorPartial[Args, ToPath, Tail]]
        def unapply[A](
            A: Type[A]
        ): Option[(?<[runtime.ArgumentLists], ?<[runtime.Path], ?<[runtime.TransformerOverrides])] =
          if (A.isCtor[runtime.TransformerOverrides.ConstructorPartial[?, ?, ?]])
            Some(
              (
                A.param_<[runtime.ArgumentLists](0),
                fixJavaEnums(A.param_<[runtime.Path](1)),
                A.param_<[runtime.TransformerOverrides](2)
              )
            )
          else scala.None
      }
      object RenamedFrom extends RenamedFromModule {
        def apply[
            FromPath <: runtime.Path: Type,
            ToPath <: runtime.Path: Type,
            Tail <: runtime.TransformerOverrides: Type
        ]: Type[runtime.TransformerOverrides.RenamedFrom[FromPath, ToPath, Tail]] =
          weakTypeTag[runtime.TransformerOverrides.RenamedFrom[FromPath, ToPath, Tail]]
        def unapply[A](A: Type[A]): Option[(?<[runtime.Path], ?<[runtime.Path], ?<[runtime.TransformerOverrides])] =
          if (A.isCtor[runtime.TransformerOverrides.RenamedFrom[?, ?, ?]])
            Some(
              (
                A.param_<[runtime.Path](0),
                fixJavaEnums(A.param_<[runtime.Path](1)),
                A.param_<[runtime.TransformerOverrides](2)
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

    object PatcherOverrides extends PatcherOverridesModule {
      val Empty: Type[runtime.PatcherOverrides.Empty] = weakTypeTag[runtime.PatcherOverrides.Empty]
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
        def apply[Init <: runtime.Path: Type, FieldName <: String: Type]: Type[runtime.Path.Select[Init, FieldName]] =
          weakTypeTag[runtime.Path.Select[Init, FieldName]]
        def unapply[A](A: Type[A]): Option[(?<[runtime.Path], ?<[String])] =
          if (A.isCtor[runtime.Path.Select[?, ?]]) Some(A.param_<[runtime.Path](0) -> A.param_<[String](1))
          else scala.None
      }
      object Matching extends MatchingModule {
        def apply[Init <: runtime.Path: Type, Subtype: Type]: Type[runtime.Path.Matching[Init, Subtype]] =
          weakTypeTag[runtime.Path.Matching[Init, Subtype]]
        def unapply[A](A: Type[A]): Option[(?<[runtime.Path], ??)] =
          if (A.isCtor[runtime.Path.Matching[?, ?]]) Some(A.param_<[runtime.Path](0) -> A.param(1))
          else scala.None
      }
      object SourceMatching extends SourceMatchingModule {
        def apply[Init <: runtime.Path: Type, Subtype: Type]: Type[runtime.Path.SourceMatching[Init, Subtype]] =
          weakTypeTag[runtime.Path.SourceMatching[Init, Subtype]]
        def unapply[A](A: Type[A]): Option[(?<[runtime.Path], ??)] =
          if (A.isCtor[runtime.Path.SourceMatching[?, ?]]) Some(A.param_<[runtime.Path](0) -> A.param(1))
          else scala.None
      }
      object EveryItem extends EveryItemModule {
        def apply[Init <: runtime.Path: Type]: Type[runtime.Path.EveryItem[Init]] =
          weakTypeTag[runtime.Path.EveryItem[Init]]
        def unapply[A](A: Type[A]): Option[?<[runtime.Path]] =
          if (A.isCtor[runtime.Path.EveryItem[?]]) Some(A.param_<[runtime.Path](0))
          else scala.None
      }
      object EveryMapKey extends EveryMapKeyModule {
        def apply[Init <: runtime.Path: Type]: Type[runtime.Path.EveryMapKey[Init]] =
          weakTypeTag[runtime.Path.EveryMapKey[Init]]
        def unapply[A](A: Type[A]): Option[?<[runtime.Path]] =
          if (A.isCtor[runtime.Path.EveryMapKey[?]]) Some(A.param_<[runtime.Path](0))
          else scala.None
      }
      object EveryMapValue extends EveryMapValueModule {
        def apply[Init <: runtime.Path: Type]: Type[runtime.Path.EveryMapValue[Init]] =
          weakTypeTag[runtime.Path.EveryMapValue[Init]]
        def unapply[A](A: Type[A]): Option[?<[runtime.Path]] =
          if (A.isCtor[runtime.Path.EveryMapValue[?]]) Some(A.param_<[runtime.Path](0))
          else scala.None
      }
    }
  }
}
