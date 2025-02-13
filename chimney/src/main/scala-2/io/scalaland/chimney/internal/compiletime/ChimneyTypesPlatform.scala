package io.scalaland.chimney.internal.compiletime

import io.scalaland.chimney.{PartialTransformer, Patcher, Transformer}
import io.scalaland.chimney.dsl as dsls
import io.scalaland.chimney.internal.runtime
import io.scalaland.chimney.integrations
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
        else A.asCtor[partial.Result[?]].map(A0 => A0.param(0))

      def Value[A: Type]: Type[partial.Result.Value[A]] = weakTypeTag[partial.Result.Value[A]]
      val Errors: Type[partial.Result.Errors] = weakTypeTag[partial.Result.Errors]
    }

    object PathElement extends PathElementModule {
      val tpe: Type[partial.PathElement] = weakTypeTag[partial.PathElement]
      val Accessor: Type[partial.PathElement.Accessor] = weakTypeTag[partial.PathElement.Accessor]
      val Index: Type[partial.PathElement.Index] = weakTypeTag[partial.PathElement.Index]
      val MapKey: Type[partial.PathElement.MapKey] = weakTypeTag[partial.PathElement.MapKey]
      val MapValue: Type[partial.PathElement.MapValue] = weakTypeTag[partial.PathElement.MapValue]
      val Const: Type[partial.PathElement.Const] = weakTypeTag[partial.PathElement.Const]
      val Computed: Type[partial.PathElement.Computed] = weakTypeTag[partial.PathElement.Computed]
    }

    val PreferTotalTransformer: Type[io.scalaland.chimney.dsl.PreferTotalTransformer.type] =
      weakTypeTag[io.scalaland.chimney.dsl.PreferTotalTransformer.type]
    val PreferPartialTransformer: Type[io.scalaland.chimney.dsl.PreferPartialTransformer.type] =
      weakTypeTag[io.scalaland.chimney.dsl.PreferPartialTransformer.type]

    val SourceOrElseFallback: Type[io.scalaland.chimney.dsl.SourceOrElseFallback.type] =
      weakTypeTag[io.scalaland.chimney.dsl.SourceOrElseFallback.type]
    val FallbackOrElseSource: Type[io.scalaland.chimney.dsl.FallbackOrElseSource.type] =
      weakTypeTag[io.scalaland.chimney.dsl.FallbackOrElseSource.type]

    val SourceAppendFallback: Type[io.scalaland.chimney.dsl.SourceAppendFallback.type] =
      weakTypeTag[io.scalaland.chimney.dsl.SourceAppendFallback.type]
    val FallbackAppendSource: Type[io.scalaland.chimney.dsl.FallbackAppendSource.type] =
      weakTypeTag[io.scalaland.chimney.dsl.FallbackAppendSource.type]

    val FailOnIgnoredSourceVal: Type[io.scalaland.chimney.dsl.FailOnIgnoredSourceVal.type] =
      weakTypeTag[io.scalaland.chimney.dsl.FailOnIgnoredSourceVal.type]

    val FailOnUnmatchedTargetSubtype: Type[io.scalaland.chimney.dsl.FailOnUnmatchedTargetSubtype.type] =
      weakTypeTag[io.scalaland.chimney.dsl.FailOnUnmatchedTargetSubtype.type]

    val RuntimeDataStore: Type[dsls.TransformerDefinitionCommons.RuntimeDataStore] =
      weakTypeTag[dsls.TransformerDefinitionCommons.RuntimeDataStore]

    object ArgumentList extends ArgumentListModule {
      val Empty: Type[runtime.ArgumentList.Empty] = weakTypeTag[runtime.ArgumentList.Empty]
      object Argument extends ArgumentModule {
        def apply[Name <: String: Type, Tpe: Type, Args <: runtime.ArgumentList: Type]
            : Type[runtime.ArgumentList.Argument[Name, Tpe, Args]] =
          weakTypeTag[runtime.ArgumentList.Argument[Name, Tpe, Args]]
        def unapply[A](A: Type[A]): Option[(?<[String], ??, ?<[runtime.ArgumentList])] =
          A.asCtor[runtime.ArgumentList.Argument[?, ?, ?]].map { A0 =>
            (A0.param_<[String](0), A0.param(1), A0.param_<[runtime.ArgumentList](2))
          }
      }
    }

    object ArgumentLists extends ArgumentListsModule {
      val Empty: Type[runtime.ArgumentLists.Empty] = weakTypeTag[runtime.ArgumentLists.Empty]
      object List extends ListModule {
        def apply[Head <: runtime.ArgumentList: Type, Tail <: runtime.ArgumentLists: Type]
            : Type[runtime.ArgumentLists.List[Head, Tail]] =
          weakTypeTag[runtime.ArgumentLists.List[Head, Tail]]
        def unapply[A](A: Type[A]): Option[(?<[runtime.ArgumentList], ?<[runtime.ArgumentLists])] =
          A.asCtor[runtime.ArgumentLists.List[?, ?]].map { A0 =>
            (A0.param_<[runtime.ArgumentList](0), A0.param_<[runtime.ArgumentLists](1))
          }
      }
    }

    object TransformerOverrides extends TransformerOverridesModule {
      val Empty: Type[runtime.TransformerOverrides.Empty] = weakTypeTag[runtime.TransformerOverrides.Empty]
      object Unused extends UnusedModule {
        def apply[FromPath <: runtime.Path: Type, Tail <: runtime.TransformerOverrides: Type]
            : Type[runtime.TransformerOverrides.Unused[FromPath, Tail]] =
          weakTypeTag[runtime.TransformerOverrides.Unused[FromPath, Tail]]
        def unapply[A](A: Type[A]): Option[(?<[runtime.Path], ?<[runtime.TransformerOverrides])] =
          A.asCtor[runtime.TransformerOverrides.Unused[?, ?]].map { A0 =>
            fixJavaEnums(A0.param_<[runtime.Path](0)) -> A0.param_<[runtime.TransformerOverrides](1)
          }
      }
      object Unmatched extends UnmatchedModule {
        def apply[ToPath <: runtime.Path: Type, Tail <: runtime.TransformerOverrides: Type]
            : Type[runtime.TransformerOverrides.Unmatched[ToPath, Tail]] =
          weakTypeTag[runtime.TransformerOverrides.Unmatched[ToPath, Tail]]
        def unapply[A](A: Type[A]): Option[(?<[runtime.Path], ?<[runtime.TransformerOverrides])] =
          A.asCtor[runtime.TransformerOverrides.Unmatched[?, ?]].map { A0 =>
            fixJavaEnums(A0.param_<[runtime.Path](0)) -> A0.param_<[runtime.TransformerOverrides](1)
          }
      }
      object Const extends ConstModule {
        def apply[ToPath <: runtime.Path: Type, Tail <: runtime.TransformerOverrides: Type]
            : Type[runtime.TransformerOverrides.Const[ToPath, Tail]] =
          weakTypeTag[runtime.TransformerOverrides.Const[ToPath, Tail]]
        def unapply[A](A: Type[A]): Option[(?<[runtime.Path], ?<[runtime.TransformerOverrides])] =
          A.asCtor[runtime.TransformerOverrides.Const[?, ?]].map { A0 =>
            fixJavaEnums(A0.param_<[runtime.Path](0)) -> A0.param_<[runtime.TransformerOverrides](1)
          }
      }
      object ConstPartial extends ConstPartialModule {
        def apply[ToPath <: runtime.Path: Type, Tail <: runtime.TransformerOverrides: Type]
            : Type[runtime.TransformerOverrides.ConstPartial[ToPath, Tail]] =
          weakTypeTag[runtime.TransformerOverrides.ConstPartial[ToPath, Tail]]
        def unapply[A](A: Type[A]): Option[(?<[runtime.Path], ?<[runtime.TransformerOverrides])] =
          A.asCtor[runtime.TransformerOverrides.ConstPartial[?, ?]].map { A0 =>
            fixJavaEnums(A0.param_<[runtime.Path](0)) -> A0.param_<[runtime.TransformerOverrides](1)
          }
      }
      object Computed extends ComputedModule {
        def apply[
            FromPath <: runtime.Path: Type,
            ToPath <: runtime.Path: Type,
            Tail <: runtime.TransformerOverrides: Type
        ]: Type[runtime.TransformerOverrides.Computed[FromPath, ToPath, Tail]] =
          weakTypeTag[runtime.TransformerOverrides.Computed[FromPath, ToPath, Tail]]
        def unapply[A](A: Type[A]): Option[(?<[runtime.Path], ?<[runtime.Path], ?<[runtime.TransformerOverrides])] =
          A.asCtor[runtime.TransformerOverrides.Computed[?, ?, ?]].map { A0 =>
            (
              fixJavaEnums(A0.param_<[runtime.Path](0)),
              fixJavaEnums(A0.param_<[runtime.Path](1)),
              A0.param_<[runtime.TransformerOverrides](2)
            )
          }
      }
      object ComputedPartial extends ComputedPartialModule {
        def apply[
            FromPath <: runtime.Path: Type,
            ToPath <: runtime.Path: Type,
            Tail <: runtime.TransformerOverrides: Type
        ]: Type[runtime.TransformerOverrides.ComputedPartial[FromPath, ToPath, Tail]] =
          weakTypeTag[runtime.TransformerOverrides.ComputedPartial[FromPath, ToPath, Tail]]
        def unapply[A](A: Type[A]): Option[(?<[runtime.Path], ?<[runtime.Path], ?<[runtime.TransformerOverrides])] =
          A.asCtor[runtime.TransformerOverrides.ComputedPartial[?, ?, ?]].map { A0 =>
            (
              fixJavaEnums(A0.param_<[runtime.Path](0)),
              fixJavaEnums(A0.param_<[runtime.Path](1)),
              A0.param_<[runtime.TransformerOverrides](2)
            )
          }
      }
      object Fallback extends FallbackModule {
        def apply[
            FallbackType: Type,
            ToPath <: runtime.Path: Type,
            Tail <: runtime.TransformerOverrides: Type
        ]: Type[runtime.TransformerOverrides.Fallback[FallbackType, ToPath, Tail]] =
          weakTypeTag[runtime.TransformerOverrides.Fallback[FallbackType, ToPath, Tail]]
        def unapply[A](A: Type[A]): Option[(??, ?<[runtime.Path], ?<[runtime.TransformerOverrides])] =
          A.asCtor[runtime.TransformerOverrides.Fallback[?, ?, ?]].map { A0 =>
            (
              A0.param(0),
              fixJavaEnums(A0.param_<[runtime.Path](1)),
              A0.param_<[runtime.TransformerOverrides](2)
            )
          }
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
          A.asCtor[runtime.TransformerOverrides.Constructor[?, ?, ?]].map { A0 =>
            (
              A0.param_<[runtime.ArgumentLists](0),
              fixJavaEnums(A0.param_<[runtime.Path](1)),
              A0.param_<[runtime.TransformerOverrides](2)
            )
          }
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
          A.asCtor[runtime.TransformerOverrides.ConstructorPartial[?, ?, ?]].map { A0 =>
            (
              A0.param_<[runtime.ArgumentLists](0),
              fixJavaEnums(A0.param_<[runtime.Path](1)),
              A0.param_<[runtime.TransformerOverrides](2)
            )
          }
      }
      object Renamed extends RenamedModule {
        def apply[
            FromPath <: runtime.Path: Type,
            ToPath <: runtime.Path: Type,
            Tail <: runtime.TransformerOverrides: Type
        ]: Type[runtime.TransformerOverrides.Renamed[FromPath, ToPath, Tail]] =
          weakTypeTag[runtime.TransformerOverrides.Renamed[FromPath, ToPath, Tail]]
        def unapply[A](A: Type[A]): Option[(?<[runtime.Path], ?<[runtime.Path], ?<[runtime.TransformerOverrides])] =
          A.asCtor[runtime.TransformerOverrides.Renamed[?, ?, ?]].map { A0 =>
            (
              A0.param_<[runtime.Path](0),
              fixJavaEnums(A0.param_<[runtime.Path](1)),
              A0.param_<[runtime.TransformerOverrides](2)
            )
          }
      }
    }

    object TransformerFlags extends TransformerFlagsModule {
      val Default: Type[runtime.TransformerFlags.Default] = weakTypeTag[runtime.TransformerFlags.Default]

      object Enable extends EnableModule {
        def apply[F <: runtime.TransformerFlags.Flag: Type, Flags <: runtime.TransformerFlags: Type]
            : Type[runtime.TransformerFlags.Enable[F, Flags]] =
          weakTypeTag[runtime.TransformerFlags.Enable[F, Flags]]
        def unapply[A](A: Type[A]): Option[(?<[runtime.TransformerFlags.Flag], ?<[runtime.TransformerFlags])] =
          A.asCtor[runtime.TransformerFlags.Enable[?, ?]].map { A0 =>
            A0.param_<[runtime.TransformerFlags.Flag](0) -> A0.param_<[runtime.TransformerFlags](1)
          }
      }
      object Disable extends DisableModule {
        def apply[F <: runtime.TransformerFlags.Flag: Type, Flags <: runtime.TransformerFlags: Type]
            : Type[runtime.TransformerFlags.Disable[F, Flags]] =
          weakTypeTag[runtime.TransformerFlags.Disable[F, Flags]]
        def unapply[A](A: Type[A]): Option[(?<[runtime.TransformerFlags.Flag], ?<[runtime.TransformerFlags])] =
          A.asCtor[runtime.TransformerFlags.Disable[?, ?]].map { A0 =>
            A0.param_<[runtime.TransformerFlags.Flag](0) -> A0.param_<[runtime.TransformerFlags](1)
          }
      }
      object Source extends SourceModule {
        def apply[
            SourcePath <: runtime.Path: Type,
            SourceFlags <: runtime.TransformerFlags: Type,
            Flags <: runtime.TransformerFlags: Type
        ]: Type[runtime.TransformerFlags.Source[SourcePath, SourceFlags, Flags]] =
          weakTypeTag[runtime.TransformerFlags.Source[SourcePath, SourceFlags, Flags]]
        def unapply[A](
            A: Type[A]
        ): Option[(?<[runtime.Path], ?<[runtime.TransformerFlags], ?<[runtime.TransformerFlags])] =
          A.asCtor[runtime.TransformerFlags.Source[?, ?, ?]].map { A0 =>
            (
              A0.param_<[runtime.Path](0),
              A0.param_<[runtime.TransformerFlags](1),
              A0.param_<[runtime.TransformerFlags](2)
            )
          }
      }
      object Target extends TargetModule {
        def apply[
            TargetPath <: runtime.Path: Type,
            TargetFlags <: runtime.TransformerFlags: Type,
            Flags <: runtime.TransformerFlags: Type
        ]: Type[runtime.TransformerFlags.Target[TargetPath, TargetFlags, Flags]] =
          weakTypeTag[runtime.TransformerFlags.Target[TargetPath, TargetFlags, Flags]]
        def unapply[A](
            A: Type[A]
        ): Option[(?<[runtime.Path], ?<[runtime.TransformerFlags], ?<[runtime.TransformerFlags])] =
          A.asCtor[runtime.TransformerFlags.Target[?, ?, ?]].map { A0 =>
            (
              A0.param_<[runtime.Path](0),
              A0.param_<[runtime.TransformerFlags](1),
              A0.param_<[runtime.TransformerFlags](2)
            )
          }
      }

      object Flags extends FlagsModule {
        val InheritedAccessors: Type[runtime.TransformerFlags.InheritedAccessors] =
          weakTypeTag[runtime.TransformerFlags.InheritedAccessors]
        val MethodAccessors: Type[runtime.TransformerFlags.MethodAccessors] =
          weakTypeTag[runtime.TransformerFlags.MethodAccessors]
        val DefaultValues: Type[runtime.TransformerFlags.DefaultValues] =
          weakTypeTag[runtime.TransformerFlags.DefaultValues]
        object DefaultValueOfType extends DefaultValueOfTypeModule {
          def apply[T: Type]: Type[runtime.TransformerFlags.DefaultValueOfType[T]] =
            weakTypeTag[runtime.TransformerFlags.DefaultValueOfType[T]]
          def unapply[A](A: Type[A]): Option[??] =
            A.asCtor[runtime.TransformerFlags.DefaultValueOfType[?]].map(A0 => A0.param(0))
        }
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
        val NonAnyValWrappers: Type[runtime.TransformerFlags.NonAnyValWrappers] =
          weakTypeTag[runtime.TransformerFlags.NonAnyValWrappers]
        val TypeConstraintEvidence: Type[runtime.TransformerFlags.TypeConstraintEvidence] =
          weakTypeTag[runtime.TransformerFlags.TypeConstraintEvidence]
        val ImplicitConversions: Type[runtime.TransformerFlags.ImplicitConversions] =
          weakTypeTag[runtime.TransformerFlags.ImplicitConversions]
        object ImplicitConflictResolution extends ImplicitConflictResolutionModule {
          def apply[R <: dsls.ImplicitTransformerPreference: Type]
              : Type[runtime.TransformerFlags.ImplicitConflictResolution[R]] =
            weakTypeTag[runtime.TransformerFlags.ImplicitConflictResolution[R]]
          def unapply[A](A: Type[A]): Option[?<[dsls.ImplicitTransformerPreference]] =
            A.asCtor[runtime.TransformerFlags.ImplicitConflictResolution[?]].map { A0 =>
              A0.param_<[dsls.ImplicitTransformerPreference](0)
            }
        }
        object OptionFallbackMerge extends OptionFallbackMergeModule {
          def apply[S <: dsls.OptionFallbackMergeStrategy: Type]
              : Type[runtime.TransformerFlags.OptionFallbackMerge[S]] =
            weakTypeTag[runtime.TransformerFlags.OptionFallbackMerge[S]]
          def unapply[A](A: Type[A]): Option[?<[dsls.OptionFallbackMergeStrategy]] =
            A.asCtor[runtime.TransformerFlags.OptionFallbackMerge[?]].map { A0 =>
              A0.param_<[dsls.OptionFallbackMergeStrategy](0)
            }
        }
        object EitherFallbackMerge extends EitherFallbackMergeModule {
          def apply[S <: dsls.OptionFallbackMergeStrategy: Type]
              : Type[runtime.TransformerFlags.EitherFallbackMerge[S]] =
            weakTypeTag[runtime.TransformerFlags.EitherFallbackMerge[S]]
          def unapply[A](A: Type[A]): Option[?<[dsls.OptionFallbackMergeStrategy]] =
            A.asCtor[runtime.TransformerFlags.EitherFallbackMerge[?]].map { A0 =>
              A0.param_<[dsls.OptionFallbackMergeStrategy](0)
            }
        }
        object CollectionFallbackMerge extends CollectionFallbackMergeModule {
          def apply[S <: dsls.CollectionFallbackMergeStrategy: Type]
              : Type[runtime.TransformerFlags.CollectionFallbackMerge[S]] =
            weakTypeTag[runtime.TransformerFlags.CollectionFallbackMerge[S]]
          def unapply[A](A: Type[A]): Option[?<[dsls.CollectionFallbackMergeStrategy]] =
            A.asCtor[runtime.TransformerFlags.CollectionFallbackMerge[?]].map { A0 =>
              A0.param_<[dsls.CollectionFallbackMergeStrategy](0)
            }
        }
        object FieldNameComparison extends FieldNameComparisonModule {
          def apply[C <: dsls.TransformedNamesComparison: Type]: Type[runtime.TransformerFlags.FieldNameComparison[C]] =
            weakTypeTag[runtime.TransformerFlags.FieldNameComparison[C]]
          def unapply[A](A: Type[A]): Option[?<[dsls.TransformedNamesComparison]] =
            A.asCtor[runtime.TransformerFlags.FieldNameComparison[?]].map { A0 =>
              A0.param_<[dsls.TransformedNamesComparison](0)
            }
        }
        object SubtypeNameComparison extends SubtypeNameComparisonModule {
          def apply[C <: dsls.TransformedNamesComparison: Type]
              : Type[runtime.TransformerFlags.SubtypeNameComparison[C]] =
            weakTypeTag[runtime.TransformerFlags.SubtypeNameComparison[C]]
          def unapply[A](A: Type[A]): Option[?<[dsls.TransformedNamesComparison]] =
            A.asCtor[runtime.TransformerFlags.SubtypeNameComparison[?]].map { A0 =>
              A0.param_<[dsls.TransformedNamesComparison](0)
            }
        }
        object UnusedFieldPolicyCheck extends UnusedFieldPolicyCheckModule {
          def apply[P <: dsls.UnusedFieldPolicy: Type]: Type[runtime.TransformerFlags.UnusedFieldPolicyCheck[P]] =
            weakTypeTag[runtime.TransformerFlags.UnusedFieldPolicyCheck[P]]
          def unapply[A](A: Type[A]): Option[?<[dsls.UnusedFieldPolicy]] =
            A.asCtor[runtime.TransformerFlags.UnusedFieldPolicyCheck[?]].map { A0 =>
              A0.param_<[dsls.UnusedFieldPolicy](0)
            }
        }
        object UnmatchedSubtypePolicyCheck extends UnmatchedSubtypePolicyCheckModule {
          def apply[P <: dsls.UnmatchedSubtypePolicy: Type]
              : Type[runtime.TransformerFlags.UnmatchedSubtypePolicyCheck[P]] =
            weakTypeTag[runtime.TransformerFlags.UnmatchedSubtypePolicyCheck[P]]
          def unapply[A](A: Type[A]): Option[?<[dsls.UnmatchedSubtypePolicy]] =
            A.asCtor[runtime.TransformerFlags.UnmatchedSubtypePolicyCheck[?]].map { A0 =>
              A0.param_<[dsls.UnmatchedSubtypePolicy](0)
            }
        }
        val MacrosLogging: Type[runtime.TransformerFlags.MacrosLogging] =
          weakTypeTag[runtime.TransformerFlags.MacrosLogging]
      }
    }

    object PatcherOverrides extends PatcherOverridesModule {
      val Empty: Type[runtime.PatcherOverrides.Empty] = weakTypeTag[runtime.PatcherOverrides.Empty]
      object Ignored extends IgnoredModule {
        def apply[PatchPath <: runtime.Path: Type, Tail <: runtime.PatcherOverrides: Type]
            : Type[runtime.PatcherOverrides.Ignored[PatchPath, Tail]] =
          weakTypeTag[runtime.PatcherOverrides.Ignored[PatchPath, Tail]]
        def unapply[A](A: Type[A]): Option[(?<[runtime.Path], ?<[runtime.PatcherOverrides])] =
          A.asCtor[runtime.PatcherOverrides.Ignored[?, ?]].map { A0 =>
            fixJavaEnums(A0.param_<[runtime.Path](0)) -> A0.param_<[runtime.PatcherOverrides](1)
          }
      }
      object Const extends ConstModule {
        def apply[
            ObjPath <: runtime.Path: Type,
            Tail <: runtime.PatcherOverrides: Type
        ]: Type[runtime.PatcherOverrides.Const[ObjPath, Tail]] =
          weakTypeTag[runtime.PatcherOverrides.Const[ObjPath, Tail]]
        def unapply[A](A: Type[A]): Option[(?<[runtime.Path], ?<[runtime.PatcherOverrides])] =
          A.asCtor[runtime.PatcherOverrides.Const[?, ?]].map { A0 =>
            (fixJavaEnums(A0.param_<[runtime.Path](0)), A0.param_<[runtime.PatcherOverrides](1))
          }
      }
      object Computed extends ComputedModule {
        def apply[
            PatchPath <: runtime.Path: Type,
            ObjPath <: runtime.Path: Type,
            Tail <: runtime.PatcherOverrides: Type
        ]: Type[runtime.PatcherOverrides.Computed[PatchPath, ObjPath, Tail]] =
          weakTypeTag[runtime.PatcherOverrides.Computed[PatchPath, ObjPath, Tail]]
        def unapply[A](A: Type[A]): Option[(?<[runtime.Path], ?<[runtime.Path], ?<[runtime.PatcherOverrides])] =
          A.asCtor[runtime.PatcherOverrides.Computed[?, ?, ?]].map { A0 =>
            (
              fixJavaEnums(A0.param_<[runtime.Path](0)),
              fixJavaEnums(A0.param_<[runtime.Path](1)),
              A0.param_<[runtime.PatcherOverrides](2)
            )
          }
      }
    }

    object PatcherFlags extends PatcherFlagsModule {
      val Default: Type[runtime.PatcherFlags.Default] = weakTypeTag[runtime.PatcherFlags.Default]

      object Enable extends EnableModule {
        def apply[F <: runtime.PatcherFlags.Flag: Type, Flags <: runtime.PatcherFlags: Type]
            : Type[runtime.PatcherFlags.Enable[F, Flags]] =
          weakTypeTag[runtime.PatcherFlags.Enable[F, Flags]]
        def unapply[A](A: Type[A]): Option[(?<[runtime.PatcherFlags.Flag], ?<[runtime.PatcherFlags])] =
          A.asCtor[runtime.PatcherFlags.Enable[?, ?]].map { A0 =>
            A0.param_<[runtime.PatcherFlags.Flag](0) -> A0.param_<[runtime.PatcherFlags](1)
          }
      }
      object Disable extends DisableModule {
        def apply[F <: runtime.PatcherFlags.Flag: Type, Flags <: runtime.PatcherFlags: Type]
            : Type[runtime.PatcherFlags.Disable[F, Flags]] =
          weakTypeTag[runtime.PatcherFlags.Disable[F, Flags]]
        def unapply[A](A: Type[A]): Option[(?<[runtime.PatcherFlags.Flag], ?<[runtime.PatcherFlags])] =
          A.asCtor[runtime.PatcherFlags.Disable[?, ?]].map { A0 =>
            A0.param_<[runtime.PatcherFlags.Flag](0) -> A0.param_<[runtime.PatcherFlags](1)
          }
      }
      object PatchedValue extends PatchedValueModule {
        def apply[
            ObjPath <: runtime.Path: Type,
            ObjFlags <: runtime.PatcherFlags: Type,
            Flags <: runtime.PatcherFlags: Type
        ]: Type[runtime.PatcherFlags.PatchedValue[ObjPath, ObjFlags, Flags]] =
          weakTypeTag[runtime.PatcherFlags.PatchedValue[ObjPath, ObjFlags, Flags]]
        def unapply[A](
            A: Type[A]
        ): Option[(?<[runtime.Path], ?<[runtime.PatcherFlags], ?<[runtime.PatcherFlags])] =
          A.asCtor[runtime.PatcherFlags.PatchedValue[?, ?, ?]].map { A0 =>
            (
              A0.param_<[runtime.Path](0),
              A0.param_<[runtime.PatcherFlags](1),
              A0.param_<[runtime.PatcherFlags](2)
            )
          }
      }

      object Flags extends FlagsModule {
        val IgnoreNoneInPatch: Type[runtime.PatcherFlags.IgnoreNoneInPatch] =
          weakTypeTag[runtime.PatcherFlags.IgnoreNoneInPatch]
        val IgnoreLeftInPatch: Type[runtime.PatcherFlags.IgnoreLeftInPatch] =
          weakTypeTag[runtime.PatcherFlags.IgnoreLeftInPatch]
        val AppendCollectionInPatch: Type[runtime.PatcherFlags.AppendCollectionInPatch] =
          weakTypeTag[runtime.PatcherFlags.AppendCollectionInPatch]
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
          A.asCtor[runtime.Path.Select[?, ?]].map(A0 => A0.param_<[runtime.Path](0) -> A0.param_<[String](1))
      }
      object Matching extends MatchingModule {
        def apply[Init <: runtime.Path: Type, Subtype: Type]: Type[runtime.Path.Matching[Init, Subtype]] =
          weakTypeTag[runtime.Path.Matching[Init, Subtype]]
        def unapply[A](A: Type[A]): Option[(?<[runtime.Path], ??)] =
          A.asCtor[runtime.Path.Matching[?, ?]].map(A0 => A0.param_<[runtime.Path](0) -> A0.param(1))
      }
      object SourceMatching extends SourceMatchingModule {
        def apply[Init <: runtime.Path: Type, Subtype: Type]: Type[runtime.Path.SourceMatching[Init, Subtype]] =
          weakTypeTag[runtime.Path.SourceMatching[Init, Subtype]]
        def unapply[A](A: Type[A]): Option[(?<[runtime.Path], ??)] =
          A.asCtor[runtime.Path.SourceMatching[?, ?]].map(A0 => A0.param_<[runtime.Path](0) -> A0.param(1))
      }
      object EveryItem extends EveryItemModule {
        def apply[Init <: runtime.Path: Type]: Type[runtime.Path.EveryItem[Init]] =
          weakTypeTag[runtime.Path.EveryItem[Init]]
        def unapply[A](A: Type[A]): Option[?<[runtime.Path]] =
          A.asCtor[runtime.Path.EveryItem[?]].map(A0 => A0.param_<[runtime.Path](0))
      }
      object EveryMapKey extends EveryMapKeyModule {
        def apply[Init <: runtime.Path: Type]: Type[runtime.Path.EveryMapKey[Init]] =
          weakTypeTag[runtime.Path.EveryMapKey[Init]]
        def unapply[A](A: Type[A]): Option[?<[runtime.Path]] =
          A.asCtor[runtime.Path.EveryMapKey[?]].map(A0 => A0.param_<[runtime.Path](0))
      }
      object EveryMapValue extends EveryMapValueModule {
        def apply[Init <: runtime.Path: Type]: Type[runtime.Path.EveryMapValue[Init]] =
          weakTypeTag[runtime.Path.EveryMapValue[Init]]
        def unapply[A](A: Type[A]): Option[?<[runtime.Path]] =
          A.asCtor[runtime.Path.EveryMapValue[?]].map(A0 => A0.param_<[runtime.Path](0))
      }
    }

    object PartialOuterTransformer extends PartialOuterTransformerModule {
      def apply[From: Type, To: Type, InnerFrom: Type, InnerTo: Type]
          : Type[integrations.PartialOuterTransformer[From, To, InnerFrom, InnerTo]] =
        weakTypeTag[integrations.PartialOuterTransformer[From, To, InnerFrom, InnerTo]]
      def unapply[A](A: Type[A]): Option[(??, ??, ??, ??)] =
        A.asCtor[integrations.PartialOuterTransformer[?, ?, ?, ?]]
          .map(A0 => (A0.param(0), A0.param(1), A0.param(2), A0.param(3)))
      def inferred[From: Type, To: Type]: ExistentialType =
        weakTypeTag[integrations.PartialOuterTransformer[From, To, ?, ?]].as_??
    }
    object TotalOuterTransformer extends TotalOuterTransformerModule {
      def apply[From: Type, To: Type, InnerFrom: Type, InnerTo: Type]
          : Type[integrations.TotalOuterTransformer[From, To, InnerFrom, InnerTo]] =
        weakTypeTag[integrations.TotalOuterTransformer[From, To, InnerFrom, InnerTo]]
      def unapply[A](A: Type[A]): Option[(??, ??, ??, ??)] =
        A.asCtor[integrations.TotalOuterTransformer[?, ?, ?, ?]]
          .map(A0 => (A0.param(0), A0.param(1), A0.param(2), A0.param(3)))
      def inferred[From: Type, To: Type]: ExistentialType =
        weakTypeTag[integrations.TotalOuterTransformer[From, To, ?, ?]].as_??
    }
    object DefaultValue extends DefaultValueModule {
      def apply[Value: Type]: Type[integrations.DefaultValue[Value]] =
        weakTypeTag[integrations.DefaultValue[Value]]
      def unapply[A](A: Type[A]): Option[??] =
        A.asCtor[integrations.DefaultValue[?]].map(A0 => A0.param(0))
    }
    object OptionalValue extends OptionalValueModule {
      def apply[Optional: Type, Value: Type]: Type[integrations.OptionalValue[Optional, Value]] =
        weakTypeTag[integrations.OptionalValue[Optional, Value]]
      def unapply[A](A: Type[A]): Option[(??, ??)] =
        A.asCtor[integrations.OptionalValue[?, ?]].map(A0 => A0.param(0) -> A0.param(1))
      def inferred[Optional: Type]: ExistentialType =
        weakTypeTag[integrations.OptionalValue[Optional, ?]].as_??
    }
    object PartiallyBuildIterable extends PartiallyBuildIterableModule {
      def apply[Collection: Type, Item: Type]: Type[integrations.PartiallyBuildIterable[Collection, Item]] =
        weakTypeTag[integrations.PartiallyBuildIterable[Collection, Item]]
      def unapply[A](A: Type[A]): Option[(??, ??)] =
        A.asCtor[integrations.PartiallyBuildIterable[?, ?]].map(A0 => A0.param(0) -> A0.param(1))
      def inferred[Collection: Type]: ExistentialType =
        weakTypeTag[integrations.PartiallyBuildIterable[Collection, ?]].as_??
    }
    object PartiallyBuildMap extends PartiallyBuildMapModule {
      def apply[Mapp: Type, Key: Type, Value: Type]: Type[integrations.PartiallyBuildMap[Mapp, Key, Value]] =
        weakTypeTag[integrations.PartiallyBuildMap[Mapp, Key, Value]]
      def unapply[A](A: Type[A]): Option[(??, ??, ??)] =
        A.asCtor[integrations.PartiallyBuildMap[?, ?, ?]].map(A0 => (A0.param(0), A0.param(1), A0.param(2)))
    }
    object TotallyBuildIterable extends TotallyBuildIterableModule {
      def apply[Collection: Type, Item: Type]: Type[integrations.TotallyBuildIterable[Collection, Item]] =
        weakTypeTag[integrations.TotallyBuildIterable[Collection, Item]]
      def unapply[A](A: Type[A]): Option[(??, ??)] =
        A.asCtor[integrations.TotallyBuildIterable[?, ?]].map(A0 => A0.param(0) -> A0.param(1))
      def inferred[Collection: Type]: ExistentialType =
        weakTypeTag[integrations.TotallyBuildIterable[Collection, ?]].as_??
    }
    object TotallyBuildMap extends TotallyBuildMapModule {
      def apply[Mapp: Type, Key: Type, Value: Type]: Type[integrations.TotallyBuildMap[Mapp, Key, Value]] =
        weakTypeTag[integrations.TotallyBuildMap[Mapp, Key, Value]]
      def unapply[A](A: Type[A]): Option[(??, ??, ??)] =
        A.asCtor[integrations.TotallyBuildMap[?, ?, ?]].map(A0 => (A0.param(0), A0.param(1), A0.param(2)))
    }
  }
}
