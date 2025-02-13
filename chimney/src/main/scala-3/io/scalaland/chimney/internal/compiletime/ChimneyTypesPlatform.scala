package io.scalaland.chimney.internal.compiletime

import io.scalaland.chimney.{PartialTransformer, Patcher, Transformer}
import io.scalaland.chimney.dsl as dsls
import io.scalaland.chimney.integrations
import io.scalaland.chimney.internal.runtime
import io.scalaland.chimney.partial

import scala.quoted

private[compiletime] trait ChimneyTypesPlatform extends ChimneyTypes { this: ChimneyDefinitionsPlatform =>

  import quotes.*, quotes.reflect.*

  object ChimneyType extends ChimneyTypeModule {

    def Transformer[From: Type, To: Type]: Type[Transformer[From, To]] = quoted.Type.of[Transformer[From, To]]
    def PartialTransformer[From: Type, To: Type]: Type[PartialTransformer[From, To]] =
      quoted.Type.of[PartialTransformer[From, To]]
    def Patcher[A: Type, Patch: Type]: Type[Patcher[A, Patch]] = quoted.Type.of[Patcher[A, Patch]]

    object PartialResult extends PartialResultModule {
      def apply[A: Type]: Type[partial.Result[A]] = quoted.Type.of[partial.Result[A]]
      def unapply[A](tpe: Type[A]): Option[??] = tpe match {
        case '[partial.Result[inner]] => Some(Type[inner].as_??)
        case _                        => scala.None
      }

      def Value[A: Type]: Type[partial.Result.Value[A]] = quoted.Type.of[partial.Result.Value[A]]
      val Errors: Type[partial.Result.Errors] = quoted.Type.of[partial.Result.Errors]
    }

    object PathElement extends PathElementModule {
      val tpe: Type[partial.PathElement] = quoted.Type.of[partial.PathElement]
      val Accessor: Type[partial.PathElement.Accessor] = quoted.Type.of[partial.PathElement.Accessor]
      val Index: Type[partial.PathElement.Index] = quoted.Type.of[partial.PathElement.Index]
      val MapKey: Type[partial.PathElement.MapKey] = quoted.Type.of[partial.PathElement.MapKey]
      val MapValue: Type[partial.PathElement.MapValue] = quoted.Type.of[partial.PathElement.MapValue]
      val Const: Type[partial.PathElement.Const] = quoted.Type.of[partial.PathElement.Const]
      val Computed: Type[partial.PathElement.Computed] = quoted.Type.of[partial.PathElement.Computed]
    }

    val PreferTotalTransformer: Type[io.scalaland.chimney.dsl.PreferTotalTransformer.type] =
      quoted.Type.of[io.scalaland.chimney.dsl.PreferTotalTransformer.type]
    val PreferPartialTransformer: Type[io.scalaland.chimney.dsl.PreferPartialTransformer.type] =
      quoted.Type.of[io.scalaland.chimney.dsl.PreferPartialTransformer.type]

    val SourceOrElseFallback: Type[io.scalaland.chimney.dsl.SourceOrElseFallback.type] =
      quoted.Type.of[io.scalaland.chimney.dsl.SourceOrElseFallback.type]
    val FallbackOrElseSource: Type[io.scalaland.chimney.dsl.FallbackOrElseSource.type] =
      quoted.Type.of[io.scalaland.chimney.dsl.FallbackOrElseSource.type]

    val SourceAppendFallback: Type[io.scalaland.chimney.dsl.SourceAppendFallback.type] =
      quoted.Type.of[io.scalaland.chimney.dsl.SourceAppendFallback.type]
    val FallbackAppendSource: Type[io.scalaland.chimney.dsl.FallbackAppendSource.type] =
      quoted.Type.of[io.scalaland.chimney.dsl.FallbackAppendSource.type]

    val FailOnIgnoredSourceVal: Type[io.scalaland.chimney.dsl.FailOnIgnoredSourceVal.type] =
      quoted.Type.of[io.scalaland.chimney.dsl.FailOnIgnoredSourceVal.type]

    val FailOnUnmatchedTargetSubtype: Type[io.scalaland.chimney.dsl.FailOnUnmatchedTargetSubtype.type] =
      quoted.Type.of[io.scalaland.chimney.dsl.FailOnUnmatchedTargetSubtype.type]

    val RuntimeDataStore: Type[dsls.TransformerDefinitionCommons.RuntimeDataStore] =
      quoted.Type.of[dsls.TransformerDefinitionCommons.RuntimeDataStore]

    object ArgumentList extends ArgumentListModule {
      val Empty: Type[runtime.ArgumentList.Empty] = quoted.Type.of[runtime.ArgumentList.Empty]
      object Argument extends ArgumentModule {
        def apply[Name <: String: Type, Tpe: Type, Args <: runtime.ArgumentList: Type]
            : Type[runtime.ArgumentList.Argument[Name, Tpe, Args]] =
          quoted.Type.of[runtime.ArgumentList.Argument[Name, Tpe, Args]]
        def unapply[A](tpe: Type[A]): Option[(?<[String], ??, ?<[runtime.ArgumentList])] = tpe match {
          case '[runtime.ArgumentList.Argument[name, tpe, args]] =>
            Some((Type[name].as_?<[String], Type[tpe].as_??, Type[args].as_?<[runtime.ArgumentList]))
          case _ => scala.None
        }
      }
    }

    object ArgumentLists extends ArgumentListsModule {
      val Empty: Type[runtime.ArgumentLists.Empty] = quoted.Type.of[runtime.ArgumentLists.Empty]
      object List extends ListModule {
        def apply[Head <: runtime.ArgumentList: Type, Tail <: runtime.ArgumentLists: Type]
            : Type[runtime.ArgumentLists.List[Head, Tail]] =
          quoted.Type.of[runtime.ArgumentLists.List[Head, Tail]]
        def unapply[A](tpe: Type[A]): Option[(?<[runtime.ArgumentList], ?<[runtime.ArgumentLists])] = tpe match {
          case '[runtime.ArgumentLists.List[head, tail]] =>
            Some((Type[head].as_?<[runtime.ArgumentList], Type[tail].as_?<[runtime.ArgumentLists]))
          case _ => scala.None
        }
      }
    }

    object TransformerOverrides extends TransformerOverridesModule {
      val Empty: Type[runtime.TransformerOverrides.Empty] = quoted.Type.of[runtime.TransformerOverrides.Empty]
      object Unused extends UnusedModule {
        def apply[FromPath <: runtime.Path: Type, Tail <: runtime.TransformerOverrides: Type]
            : Type[runtime.TransformerOverrides.Unused[FromPath, Tail]] =
          quoted.Type.of[runtime.TransformerOverrides.Unused[FromPath, Tail]]
        def unapply[A](tpe: Type[A]): Option[(?<[runtime.Path], ?<[runtime.TransformerOverrides])] = tpe match {
          case '[runtime.TransformerOverrides.Unused[fromPath, cfg]] =>
            Some((Type[fromPath].as_?<[runtime.Path], Type[cfg].as_?<[runtime.TransformerOverrides]))
          case _ => scala.None
        }
      }
      object Unmatched extends UnmatchedModule {
        def apply[ToPath <: runtime.Path: Type, Tail <: runtime.TransformerOverrides: Type]
            : Type[runtime.TransformerOverrides.Unmatched[ToPath, Tail]] =
          quoted.Type.of[runtime.TransformerOverrides.Unmatched[ToPath, Tail]]
        def unapply[A](tpe: Type[A]): Option[(?<[runtime.Path], ?<[runtime.TransformerOverrides])] = tpe match {
          case '[runtime.TransformerOverrides.Unmatched[toPath, cfg]] =>
            Some((Type[toPath].as_?<[runtime.Path], Type[cfg].as_?<[runtime.TransformerOverrides]))
          case _ => scala.None
        }
      }
      object Const extends ConstModule {
        def apply[ToPath <: runtime.Path: Type, Tail <: runtime.TransformerOverrides: Type]
            : Type[runtime.TransformerOverrides.Const[ToPath, Tail]] =
          quoted.Type.of[runtime.TransformerOverrides.Const[ToPath, Tail]]
        def unapply[A](tpe: Type[A]): Option[(?<[runtime.Path], ?<[runtime.TransformerOverrides])] = tpe match {
          case '[runtime.TransformerOverrides.Const[toPath, cfg]] =>
            Some((Type[toPath].as_?<[runtime.Path], Type[cfg].as_?<[runtime.TransformerOverrides]))
          case _ => scala.None
        }
      }
      object ConstPartial extends ConstPartialModule {
        def apply[ToPath <: runtime.Path: Type, Tail <: runtime.TransformerOverrides: Type]
            : Type[runtime.TransformerOverrides.ConstPartial[ToPath, Tail]] =
          quoted.Type.of[runtime.TransformerOverrides.ConstPartial[ToPath, Tail]]
        def unapply[A](tpe: Type[A]): Option[(?<[runtime.Path], ?<[runtime.TransformerOverrides])] = tpe match {
          case '[runtime.TransformerOverrides.ConstPartial[toPath, cfg]] =>
            Some((Type[toPath].as_?<[runtime.Path], Type[cfg].as_?<[runtime.TransformerOverrides]))
          case _ => scala.None
        }
      }
      object Computed extends ComputedModule {
        def apply[
            FromPath <: runtime.Path: Type,
            ToPath <: runtime.Path: Type,
            Tail <: runtime.TransformerOverrides: Type
        ]: Type[runtime.TransformerOverrides.Computed[FromPath, ToPath, Tail]] =
          quoted.Type.of[runtime.TransformerOverrides.Computed[FromPath, ToPath, Tail]]
        def unapply[A](tpe: Type[A]): Option[(?<[runtime.Path], ?<[runtime.Path], ?<[runtime.TransformerOverrides])] =
          tpe match {
            case '[runtime.TransformerOverrides.Computed[fromPath, toPath, cfg]] =>
              Some(
                (
                  Type[fromPath].as_?<[runtime.Path],
                  Type[toPath].as_?<[runtime.Path],
                  Type[cfg].as_?<[runtime.TransformerOverrides]
                )
              )
            case _ => scala.None
          }
      }
      object ComputedPartial extends ComputedPartialModule {
        def apply[
            FromPath <: runtime.Path: Type,
            ToPath <: runtime.Path: Type,
            Tail <: runtime.TransformerOverrides: Type
        ]: Type[runtime.TransformerOverrides.ComputedPartial[FromPath, ToPath, Tail]] =
          quoted.Type.of[runtime.TransformerOverrides.ComputedPartial[FromPath, ToPath, Tail]]
        def unapply[A](tpe: Type[A]): Option[(?<[runtime.Path], ?<[runtime.Path], ?<[runtime.TransformerOverrides])] =
          tpe match {
            case '[runtime.TransformerOverrides.ComputedPartial[fromPath, toPath, cfg]] =>
              Some(
                (
                  Type[fromPath].as_?<[runtime.Path],
                  Type[toPath].as_?<[runtime.Path],
                  Type[cfg].as_?<[runtime.TransformerOverrides]
                )
              )
            case _ => scala.None
          }
      }
      object Fallback extends FallbackModule {
        def apply[
            FallbackType: Type,
            ToPath <: runtime.Path: Type,
            Tail <: runtime.TransformerOverrides: Type
        ]: Type[runtime.TransformerOverrides.Fallback[FallbackType, ToPath, Tail]] =
          quoted.Type.of[runtime.TransformerOverrides.Fallback[FallbackType, ToPath, Tail]]
        def unapply[A](tpe: Type[A]): Option[(??, ?<[runtime.Path], ?<[runtime.TransformerOverrides])] = tpe match {
          case '[runtime.TransformerOverrides.Fallback[fallbackType, toPath, cfg]] =>
            Some(
              (
                Type[fallbackType].as_??,
                Type[toPath].as_?<[runtime.Path],
                Type[cfg].as_?<[runtime.TransformerOverrides]
              )
            )
          case _ => scala.None
        }
      }
      object Constructor extends ConstructorModule {
        def apply[
            Args <: runtime.ArgumentLists: Type,
            ToPath <: runtime.Path: Type,
            Tail <: runtime.TransformerOverrides: Type
        ]: Type[runtime.TransformerOverrides.Constructor[Args, ToPath, Tail]] =
          quoted.Type.of[runtime.TransformerOverrides.Constructor[Args, ToPath, Tail]]
        def unapply[A](
            tpe: Type[A]
        ): Option[(?<[runtime.ArgumentLists], ?<[runtime.Path], ?<[runtime.TransformerOverrides])] = tpe match {
          case '[runtime.TransformerOverrides.Constructor[args, toPath, cfg]] =>
            Some(
              (
                Type[args].as_?<[runtime.ArgumentLists],
                Type[toPath].as_?<[runtime.Path],
                Type[cfg].as_?<[runtime.TransformerOverrides]
              )
            )
          case _ => scala.None
        }
      }
      object ConstructorPartial extends ConstructorPartialModule {
        def apply[
            Args <: runtime.ArgumentLists: Type,
            ToPath <: runtime.Path: Type,
            Tail <: runtime.TransformerOverrides: Type
        ]: Type[runtime.TransformerOverrides.ConstructorPartial[Args, ToPath, Tail]] =
          quoted.Type.of[runtime.TransformerOverrides.ConstructorPartial[Args, ToPath, Tail]]
        def unapply[A](
            tpe: Type[A]
        ): Option[(?<[runtime.ArgumentLists], ?<[runtime.Path], ?<[runtime.TransformerOverrides])] = tpe match {
          case '[runtime.TransformerOverrides.ConstructorPartial[args, toPath, cfg]] =>
            Some(
              (
                Type[args].as_?<[runtime.ArgumentLists],
                Type[toPath].as_?<[runtime.Path],
                Type[cfg].as_?<[runtime.TransformerOverrides]
              )
            )
          case _ => scala.None
        }
      }
      object Renamed extends RenamedModule {
        def apply[
            FromPath <: runtime.Path: Type,
            ToPath <: runtime.Path: Type,
            Tail <: runtime.TransformerOverrides: Type
        ]: Type[runtime.TransformerOverrides.Renamed[FromPath, ToPath, Tail]] =
          quoted.Type.of[runtime.TransformerOverrides.Renamed[FromPath, ToPath, Tail]]
        def unapply[A](tpe: Type[A]): Option[(?<[runtime.Path], ?<[runtime.Path], ?<[runtime.TransformerOverrides])] =
          tpe match {
            case '[runtime.TransformerOverrides.Renamed[fromPath, toPath, cfg]] =>
              Some(
                (
                  Type[fromPath].as_?<[runtime.Path],
                  Type[toPath].as_?<[runtime.Path],
                  Type[cfg].as_?<[runtime.TransformerOverrides]
                )
              )
            case _ => scala.None
          }
      }
    }

    object TransformerFlags extends TransformerFlagsModule {
      val Default: Type[runtime.TransformerFlags.Default] = quoted.Type.of[runtime.TransformerFlags.Default]

      object Enable extends EnableModule {
        def apply[F <: runtime.TransformerFlags.Flag: Type, Flags <: runtime.TransformerFlags: Type]
            : Type[runtime.TransformerFlags.Enable[F, Flags]] =
          quoted.Type.of[runtime.TransformerFlags.Enable[F, Flags]]
        def unapply[A](tpe: Type[A]): Option[(?<[runtime.TransformerFlags.Flag], ?<[runtime.TransformerFlags])] =
          tpe match {
            case '[runtime.TransformerFlags.Enable[f, flags]] =>
              Some((Type[f].as_?<[runtime.TransformerFlags.Flag], Type[flags].as_?<[runtime.TransformerFlags]))
            case _ => scala.None
          }
      }
      object Disable extends DisableModule {
        def apply[F <: runtime.TransformerFlags.Flag: Type, Flags <: runtime.TransformerFlags: Type]
            : Type[runtime.TransformerFlags.Disable[F, Flags]] =
          quoted.Type.of[runtime.TransformerFlags.Disable[F, Flags]]
        def unapply[A](tpe: Type[A]): Option[(?<[runtime.TransformerFlags.Flag], ?<[runtime.TransformerFlags])] =
          tpe match {
            case '[runtime.TransformerFlags.Disable[f, flags]] =>
              Some((Type[f].as_?<[runtime.TransformerFlags.Flag], Type[flags].as_?<[runtime.TransformerFlags]))
            case _ => scala.None
          }
      }
      object Source extends SourceModule {
        def apply[
            SourcePath <: runtime.Path: Type,
            SourceFlags <: runtime.TransformerFlags: Type,
            Flags <: runtime.TransformerFlags: Type
        ]: Type[runtime.TransformerFlags.Source[SourcePath, SourceFlags, Flags]] =
          quoted.Type.of[runtime.TransformerFlags.Source[SourcePath, SourceFlags, Flags]]
        def unapply[A](
            tpe: Type[A]
        ): Option[(?<[runtime.Path], ?<[runtime.TransformerFlags], ?<[runtime.TransformerFlags])] =
          tpe match {
            case '[runtime.TransformerFlags.Source[sourcePath, sourceFlags, flags]] =>
              Some(
                (
                  Type[sourcePath].as_?<[runtime.Path],
                  Type[sourceFlags].as_?<[runtime.TransformerFlags],
                  Type[flags].as_?<[runtime.TransformerFlags]
                )
              )
            case _ => scala.None
          }
      }
      object Target extends TargetModule {
        def apply[
            TargetPath <: runtime.Path: Type,
            TargetFlags <: runtime.TransformerFlags: Type,
            Flags <: runtime.TransformerFlags: Type
        ]: Type[runtime.TransformerFlags.Target[TargetPath, TargetFlags, Flags]] =
          quoted.Type.of[runtime.TransformerFlags.Target[TargetPath, TargetFlags, Flags]]
        def unapply[A](
            tpe: Type[A]
        ): Option[(?<[runtime.Path], ?<[runtime.TransformerFlags], ?<[runtime.TransformerFlags])] =
          tpe match {
            case '[runtime.TransformerFlags.Target[targetPath, targetFlags, flags]] =>
              Some(
                (
                  Type[targetPath].as_?<[runtime.Path],
                  Type[targetFlags].as_?<[runtime.TransformerFlags],
                  Type[flags].as_?<[runtime.TransformerFlags]
                )
              )
            case _ => scala.None
          }
      }

      object Flags extends FlagsModule {
        val InheritedAccessors: Type[runtime.TransformerFlags.InheritedAccessors] =
          quoted.Type.of[runtime.TransformerFlags.InheritedAccessors]
        val MethodAccessors: Type[runtime.TransformerFlags.MethodAccessors] =
          quoted.Type.of[runtime.TransformerFlags.MethodAccessors]
        val DefaultValues: Type[runtime.TransformerFlags.DefaultValues] =
          quoted.Type.of[runtime.TransformerFlags.DefaultValues]
        object DefaultValueOfType extends DefaultValueOfTypeModule {
          def apply[T: Type]: Type[runtime.TransformerFlags.DefaultValueOfType[T]] =
            quoted.Type.of[runtime.TransformerFlags.DefaultValueOfType[T]]
          def unapply[A](tpe: Type[A]): Option[??] = tpe match {
            case '[runtime.TransformerFlags.DefaultValueOfType[t]] => Some(Type[t].as_??)
            case _                                                 => scala.None
          }
        }
        val BeanGetters: Type[runtime.TransformerFlags.BeanGetters] =
          quoted.Type.of[runtime.TransformerFlags.BeanGetters]
        val BeanSetters: Type[runtime.TransformerFlags.BeanSetters] =
          quoted.Type.of[runtime.TransformerFlags.BeanSetters]
        val BeanSettersIgnoreUnmatched: Type[runtime.TransformerFlags.BeanSettersIgnoreUnmatched] =
          quoted.Type.of[runtime.TransformerFlags.BeanSettersIgnoreUnmatched]
        val NonUnitBeanSetters: Type[runtime.TransformerFlags.NonUnitBeanSetters] =
          quoted.Type.of[runtime.TransformerFlags.NonUnitBeanSetters]
        val OptionDefaultsToNone: Type[runtime.TransformerFlags.OptionDefaultsToNone] =
          quoted.Type.of[runtime.TransformerFlags.OptionDefaultsToNone]
        val PartialUnwrapsOption: Type[runtime.TransformerFlags.PartialUnwrapsOption] =
          quoted.Type.of[runtime.TransformerFlags.PartialUnwrapsOption]
        val NonAnyValWrappers: Type[runtime.TransformerFlags.NonAnyValWrappers] =
          quoted.Type.of[runtime.TransformerFlags.NonAnyValWrappers]
        val TypeConstraintEvidence: Type[runtime.TransformerFlags.TypeConstraintEvidence] =
          quoted.Type.of[runtime.TransformerFlags.TypeConstraintEvidence]
        val ImplicitConversions: Type[runtime.TransformerFlags.ImplicitConversions] =
          quoted.Type.of[runtime.TransformerFlags.ImplicitConversions]
        object ImplicitConflictResolution extends ImplicitConflictResolutionModule {
          def apply[R <: dsls.ImplicitTransformerPreference: Type]
              : Type[runtime.TransformerFlags.ImplicitConflictResolution[R]] =
            quoted.Type.of[runtime.TransformerFlags.ImplicitConflictResolution[R]]
          def unapply[A](tpe: Type[A]): Option[?<[dsls.ImplicitTransformerPreference]] = tpe match {
            case '[runtime.TransformerFlags.ImplicitConflictResolution[r]] =>
              Some(Type[r].as_?<[dsls.ImplicitTransformerPreference])
            case _ => scala.None
          }
        }
        object OptionFallbackMerge extends OptionFallbackMergeModule {
          def apply[S <: dsls.OptionFallbackMergeStrategy: Type]
              : Type[runtime.TransformerFlags.OptionFallbackMerge[S]] =
            quoted.Type.of[runtime.TransformerFlags.OptionFallbackMerge[S]]
          def unapply[A](tpe: Type[A]): Option[?<[dsls.OptionFallbackMergeStrategy]] = tpe match {
            case '[runtime.TransformerFlags.OptionFallbackMerge[s]] =>
              Some(Type[s].as_?<[dsls.OptionFallbackMergeStrategy])
            case _ => scala.None
          }
        }
        object EitherFallbackMerge extends EitherFallbackMergeModule {
          def apply[S <: dsls.OptionFallbackMergeStrategy: Type]
              : Type[runtime.TransformerFlags.EitherFallbackMerge[S]] =
            quoted.Type.of[runtime.TransformerFlags.EitherFallbackMerge[S]]
          def unapply[A](tpe: Type[A]): Option[?<[dsls.OptionFallbackMergeStrategy]] = tpe match {
            case '[runtime.TransformerFlags.EitherFallbackMerge[s]] =>
              Some(Type[s].as_?<[dsls.OptionFallbackMergeStrategy])
            case _ => scala.None
          }
        }
        object CollectionFallbackMerge extends CollectionFallbackMergeModule {
          def apply[S <: dsls.CollectionFallbackMergeStrategy: Type]
              : Type[runtime.TransformerFlags.CollectionFallbackMerge[S]] =
            quoted.Type.of[runtime.TransformerFlags.CollectionFallbackMerge[S]]
          def unapply[A](tpe: Type[A]): Option[?<[dsls.CollectionFallbackMergeStrategy]] = tpe match {
            case '[runtime.TransformerFlags.CollectionFallbackMerge[s]] =>
              Some(Type[s].as_?<[dsls.CollectionFallbackMergeStrategy])
            case _ => scala.None
          }
        }
        object FieldNameComparison extends FieldNameComparisonModule {
          def apply[C <: dsls.TransformedNamesComparison: Type]: Type[runtime.TransformerFlags.FieldNameComparison[C]] =
            quoted.Type.of[runtime.TransformerFlags.FieldNameComparison[C]]
          def unapply[A](tpe: Type[A]): Option[?<[dsls.TransformedNamesComparison]] = tpe match {
            case '[runtime.TransformerFlags.FieldNameComparison[c]] =>
              Some(Type[c].as_?<[dsls.TransformedNamesComparison])
            case _ => scala.None
          }
        }
        object SubtypeNameComparison extends SubtypeNameComparisonModule {
          def apply[C <: dsls.TransformedNamesComparison: Type]
              : Type[runtime.TransformerFlags.SubtypeNameComparison[C]] =
            quoted.Type.of[runtime.TransformerFlags.SubtypeNameComparison[C]]
          def unapply[A](tpe: Type[A]): Option[?<[dsls.TransformedNamesComparison]] = tpe match {
            case '[runtime.TransformerFlags.SubtypeNameComparison[c]] =>
              Some(Type[c].as_?<[dsls.TransformedNamesComparison])
            case _ => scala.None
          }
        }
        object UnusedFieldPolicyCheck extends UnusedFieldPolicyCheckModule {
          def apply[P <: dsls.UnusedFieldPolicy: Type]: Type[runtime.TransformerFlags.UnusedFieldPolicyCheck[P]] =
            quoted.Type.of[runtime.TransformerFlags.UnusedFieldPolicyCheck[P]]
          def unapply[A](tpe: Type[A]): Option[?<[dsls.UnusedFieldPolicy]] = tpe match {
            case '[runtime.TransformerFlags.UnusedFieldPolicyCheck[p]] =>
              Some(Type[p].as_?<[dsls.UnusedFieldPolicy])
            case _ => scala.None
          }
        }
        object UnmatchedSubtypePolicyCheck extends UnmatchedSubtypePolicyCheckModule {
          def apply[P <: dsls.UnmatchedSubtypePolicy: Type]
              : Type[runtime.TransformerFlags.UnmatchedSubtypePolicyCheck[P]] =
            quoted.Type.of[runtime.TransformerFlags.UnmatchedSubtypePolicyCheck[P]]
          def unapply[A](tpe: Type[A]): Option[?<[dsls.UnmatchedSubtypePolicy]] = tpe match {
            case '[runtime.TransformerFlags.UnmatchedSubtypePolicyCheck[p]] =>
              Some(Type[p].as_?<[dsls.UnmatchedSubtypePolicy])
            case _ => scala.None
          }
        }
        val MacrosLogging: Type[runtime.TransformerFlags.MacrosLogging] =
          quoted.Type.of[runtime.TransformerFlags.MacrosLogging]
      }
    }

    object PatcherOverrides extends PatcherOverridesModule {
      val Empty: Type[runtime.PatcherOverrides.Empty] = quoted.Type.of[runtime.PatcherOverrides.Empty]
      object Ignored extends IgnoredModule {
        def apply[PatchPath <: runtime.Path: Type, Tail <: runtime.PatcherOverrides: Type]
            : Type[runtime.PatcherOverrides.Ignored[PatchPath, Tail]] =
          quoted.Type.of[runtime.PatcherOverrides.Ignored[PatchPath, Tail]]
        def unapply[A](tpe: Type[A]): Option[(?<[runtime.Path], ?<[runtime.PatcherOverrides])] = tpe match {
          case '[runtime.PatcherOverrides.Ignored[patchPath, cfg]] =>
            Some((Type[patchPath].as_?<[runtime.Path], Type[cfg].as_?<[runtime.PatcherOverrides]))
          case _ => scala.None
        }
      }
      object Const extends ConstModule {
        def apply[
            ObjPath <: runtime.Path: Type,
            Tail <: runtime.PatcherOverrides: Type
        ]: Type[runtime.PatcherOverrides.Const[ObjPath, Tail]] =
          quoted.Type.of[runtime.PatcherOverrides.Const[ObjPath, Tail]]
        def unapply[A](tpe: Type[A]): Option[(?<[runtime.Path], ?<[runtime.PatcherOverrides])] =
          tpe match {
            case '[runtime.PatcherOverrides.Const[objPath, cfg]] =>
              Some((Type[objPath].as_?<[runtime.Path], Type[cfg].as_?<[runtime.PatcherOverrides]))
            case _ => scala.None
          }
      }
      object Computed extends ComputedModule {
        def apply[
            PatchPath <: runtime.Path: Type,
            ObjPath <: runtime.Path: Type,
            Tail <: runtime.PatcherOverrides: Type
        ]: Type[runtime.PatcherOverrides.Computed[PatchPath, ObjPath, Tail]] =
          quoted.Type.of[runtime.PatcherOverrides.Computed[PatchPath, ObjPath, Tail]]
        def unapply[A](tpe: Type[A]): Option[(?<[runtime.Path], ?<[runtime.Path], ?<[runtime.PatcherOverrides])] =
          tpe match {
            case '[runtime.PatcherOverrides.Computed[patchPath, objPath, cfg]] =>
              Some(
                (
                  Type[patchPath].as_?<[runtime.Path],
                  Type[objPath].as_?<[runtime.Path],
                  Type[cfg].as_?<[runtime.PatcherOverrides]
                )
              )
            case _ => scala.None
          }
      }
    }

    object PatcherFlags extends PatcherFlagsModule {
      val Default: Type[runtime.PatcherFlags.Default] = quoted.Type.of[runtime.PatcherFlags.Default]

      object Enable extends EnableModule {
        def apply[F <: runtime.PatcherFlags.Flag: Type, Flags <: runtime.PatcherFlags: Type]
            : Type[runtime.PatcherFlags.Enable[F, Flags]] =
          quoted.Type.of[runtime.PatcherFlags.Enable[F, Flags]]

        def unapply[A](tpe: Type[A]): Option[(?<[runtime.PatcherFlags.Flag], ?<[runtime.PatcherFlags])] =
          tpe match {
            case '[runtime.PatcherFlags.Enable[f, flags]] =>
              Some((Type[f].as_?<[runtime.PatcherFlags.Flag], Type[flags].as_?<[runtime.PatcherFlags]))
            case _ => scala.None
          }
      }
      object Disable extends DisableModule {
        def apply[F <: runtime.PatcherFlags.Flag: Type, Flags <: runtime.PatcherFlags: Type]
            : Type[runtime.PatcherFlags.Disable[F, Flags]] =
          quoted.Type.of[runtime.PatcherFlags.Disable[F, Flags]]

        def unapply[A](tpe: Type[A]): Option[(?<[runtime.PatcherFlags.Flag], ?<[runtime.PatcherFlags])] =
          tpe match {
            case '[runtime.PatcherFlags.Disable[f, flags]] =>
              Some((Type[f].as_?<[runtime.PatcherFlags.Flag], Type[flags].as_?<[runtime.PatcherFlags]))
            case _ => scala.None
          }
      }
      object PatchedValue extends PatchedValueModule {
        def apply[
            ObjPath <: runtime.Path: Type,
            ObjFlags <: runtime.PatcherFlags: Type,
            Flags <: runtime.PatcherFlags: Type
        ]: Type[runtime.PatcherFlags.PatchedValue[ObjPath, ObjFlags, Flags]] =
          quoted.Type.of[runtime.PatcherFlags.PatchedValue[ObjPath, ObjFlags, Flags]]
        def unapply[A](
            tpe: Type[A]
        ): Option[(?<[runtime.Path], ?<[runtime.PatcherFlags], ?<[runtime.PatcherFlags])] =
          tpe match {
            case '[runtime.PatcherFlags.PatchedValue[objPath, objFlags, flags]] =>
              Some(
                (
                  Type[objPath].as_?<[runtime.Path],
                  Type[objFlags].as_?<[runtime.PatcherFlags],
                  Type[flags].as_?<[runtime.PatcherFlags]
                )
              )
            case _ => scala.None
          }
      }

      object Flags extends FlagsModule {
        val IgnoreNoneInPatch: Type[runtime.PatcherFlags.IgnoreNoneInPatch] =
          quoted.Type.of[runtime.PatcherFlags.IgnoreNoneInPatch]
        val IgnoreLeftInPatch: Type[runtime.PatcherFlags.IgnoreLeftInPatch] =
          quoted.Type.of[runtime.PatcherFlags.IgnoreLeftInPatch]
        val AppendCollectionInPatch: Type[runtime.PatcherFlags.AppendCollectionInPatch] =
          quoted.Type.of[runtime.PatcherFlags.AppendCollectionInPatch]
        val IgnoreRedundantPatcherFields: Type[runtime.PatcherFlags.IgnoreRedundantPatcherFields] =
          quoted.Type.of[runtime.PatcherFlags.IgnoreRedundantPatcherFields]
        val MacrosLogging: Type[runtime.PatcherFlags.MacrosLogging] = quoted.Type.of[runtime.PatcherFlags.MacrosLogging]
      }
    }

    object Path extends PathModule {
      val Root: Type[runtime.Path.Root] = quoted.Type.of[runtime.Path.Root]
      object Select extends SelectModule {
        def apply[Init <: runtime.Path: Type, FieldName <: String: Type]: Type[runtime.Path.Select[Init, FieldName]] =
          quoted.Type.of[runtime.Path.Select[Init, FieldName]]
        def unapply[A](tpe: Type[A]): Option[(?<[runtime.Path], ?<[String])] = tpe match {
          case '[runtime.Path.Select[init, fieldName]] =>
            Some((Type[init].as_?<[runtime.Path], Type[fieldName].as_?<[String]))
          case _ => scala.None
        }
      }
      object Matching extends MatchingModule {
        def apply[Init <: runtime.Path: Type, Subtype: Type]: Type[runtime.Path.Matching[Init, Subtype]] =
          quoted.Type.of[runtime.Path.Matching[Init, Subtype]]
        def unapply[A](tpe: Type[A]): Option[(?<[runtime.Path], ??)] = tpe match {
          case '[runtime.Path.Matching[init, subtype]] => Some((Type[init].as_?<[runtime.Path], Type[subtype].as_??))
          case _                                       => scala.None
        }
      }
      object SourceMatching extends SourceMatchingModule {
        def apply[Init <: runtime.Path: Type, Subtype: Type]: Type[runtime.Path.SourceMatching[Init, Subtype]] =
          quoted.Type.of[runtime.Path.SourceMatching[Init, Subtype]]
        def unapply[A](tpe: Type[A]): Option[(?<[runtime.Path], ??)] = tpe match {
          case '[runtime.Path.SourceMatching[init, subtype]] =>
            Some((Type[init].as_?<[runtime.Path], Type[subtype].as_??))
          case _ => scala.None
        }
      }
      object EveryItem extends EveryItemModule {
        def apply[Init <: runtime.Path: Type]: Type[runtime.Path.EveryItem[Init]] =
          quoted.Type.of[runtime.Path.EveryItem[Init]]
        def unapply[A](tpe: Type[A]): Option[?<[runtime.Path]] = tpe match {
          case '[runtime.Path.EveryItem[init]] => Some(Type[init].as_?<[runtime.Path])
          case _                               => scala.None
        }
      }
      object EveryMapKey extends EveryMapKeyModule {
        def apply[Init <: runtime.Path: Type]: Type[runtime.Path.EveryMapKey[Init]] =
          quoted.Type.of[runtime.Path.EveryMapKey[Init]]
        def unapply[A](tpe: Type[A]): Option[?<[runtime.Path]] = tpe match {
          case '[runtime.Path.EveryMapKey[init]] => Some(Type[init].as_?<[runtime.Path])
          case _                                 => scala.None
        }
      }
      object EveryMapValue extends EveryMapValueModule {
        def apply[Init <: runtime.Path: Type]: Type[runtime.Path.EveryMapValue[Init]] =
          quoted.Type.of[runtime.Path.EveryMapValue[Init]]
        def unapply[A](tpe: Type[A]): Option[?<[runtime.Path]] = tpe match {
          case '[runtime.Path.EveryMapValue[init]] => Some(Type[init].as_?<[runtime.Path])
          case _                                   => scala.None
        }
      }
    }

    object PartialOuterTransformer extends PartialOuterTransformerModule {
      def apply[From: Type, To: Type, InnerFrom: Type, InnerTo: Type]
          : Type[integrations.PartialOuterTransformer[From, To, InnerFrom, InnerTo]] =
        quoted.Type.of[integrations.PartialOuterTransformer[From, To, InnerFrom, InnerTo]]
      def unapply[A](tpe: Type[A]): Option[(??, ??, ??, ??)] = tpe match {
        case '[integrations.PartialOuterTransformer[from, to, innerFrom, innerTo]] =>
          Some((Type[from].as_??, Type[to].as_??, Type[innerFrom].as_??, Type[innerTo].as_??))
        case _ => None
      }
      def inferred[From: Type, To: Type]: ExistentialType =
        quoted.Type.of[integrations.PartialOuterTransformer[From, To, ?, ?]].as_??
    }
    object TotalOuterTransformer extends TotalOuterTransformerModule {
      def apply[From: Type, To: Type, InnerFrom: Type, InnerTo: Type]
          : Type[integrations.TotalOuterTransformer[From, To, InnerFrom, InnerTo]] =
        quoted.Type.of[integrations.TotalOuterTransformer[From, To, InnerFrom, InnerTo]]
      def unapply[A](tpe: Type[A]): Option[(??, ??, ??, ??)] = tpe match {
        case '[integrations.TotalOuterTransformer[from, to, innerFrom, innerTo]] =>
          Some((Type[from].as_??, Type[to].as_??, Type[innerFrom].as_??, Type[innerTo].as_??))
        case _ => None
      }
      def inferred[From: Type, To: Type]: ExistentialType =
        quoted.Type.of[integrations.TotalOuterTransformer[From, To, ?, ?]].as_??
    }
    object DefaultValue extends DefaultValueModule {
      def apply[Value: Type]: Type[integrations.DefaultValue[Value]] =
        quoted.Type.of[integrations.DefaultValue[Value]]
      def unapply[A](tpe: Type[A]): Option[??] = tpe match {
        case '[integrations.DefaultValue[value]] => Some(Type[value].as_??)
        case _                                   => scala.None
      }
    }
    object OptionalValue extends OptionalValueModule {
      def apply[Optional: Type, Value: Type]: Type[integrations.OptionalValue[Optional, Value]] =
        quoted.Type.of[integrations.OptionalValue[Optional, Value]]
      def unapply[A](tpe: Type[A]): Option[(??, ??)] = tpe match {
        case '[integrations.OptionalValue[optional, value]] => Some((Type[optional].as_??, Type[value].as_??))
        case _                                              => scala.None
      }
      def inferred[Optional: Type]: ExistentialType =
        quoted.Type.of[integrations.OptionalValue[Optional, ?]].as_??
    }
    object PartiallyBuildIterable extends PartiallyBuildIterableModule {
      def apply[Collection: Type, Item: Type]: Type[integrations.PartiallyBuildIterable[Collection, Item]] =
        quoted.Type.of[integrations.PartiallyBuildIterable[Collection, Item]]
      def unapply[A](tpe: Type[A]): Option[(??, ??)] = tpe match {
        case '[integrations.PartiallyBuildIterable[collection, item]] =>
          Some((Type[collection].as_??, Type[item].as_??))
        case _ => scala.None
      }
      def inferred[Collection: Type]: ExistentialType =
        quoted.Type.of[integrations.PartiallyBuildIterable[Collection, ?]].as_??
    }
    object PartiallyBuildMap extends PartiallyBuildMapModule {
      def apply[Map: Type, Key: Type, Value: Type]: Type[integrations.PartiallyBuildMap[Map, Key, Value]] =
        quoted.Type.of[integrations.PartiallyBuildMap[Map, Key, Value]]
      def unapply[A](tpe: Type[A]): Option[(??, ??, ??)] = tpe match {
        case '[integrations.PartiallyBuildMap[map, key, value]] =>
          Some((Type[map].as_??, Type[key].as_??, Type[value].as_??))
        case _ => scala.None
      }
    }
    object TotallyBuildIterable extends TotallyBuildIterableModule {
      def apply[Collection: Type, Item: Type]: Type[integrations.TotallyBuildIterable[Collection, Item]] =
        quoted.Type.of[integrations.TotallyBuildIterable[Collection, Item]]
      def unapply[A](tpe: Type[A]): Option[(??, ??)] = tpe match {
        case '[integrations.TotallyBuildIterable[collection, item]] =>
          Some((Type[collection].as_??, Type[item].as_??))
        case _ => scala.None
      }
      def inferred[Collection: Type]: ExistentialType =
        quoted.Type.of[integrations.TotallyBuildIterable[Collection, ?]].as_??
    }
    object TotallyBuildMap extends TotallyBuildMapModule {
      def apply[Map: Type, Key: Type, Value: Type]: Type[integrations.TotallyBuildMap[Map, Key, Value]] =
        quoted.Type.of[integrations.TotallyBuildMap[Map, Key, Value]]
      def unapply[A](tpe: Type[A]): Option[(??, ??, ??)] = tpe match {
        case '[integrations.TotallyBuildMap[map, key, value]] =>
          Some((Type[map].as_??, Type[key].as_??, Type[value].as_??))
        case _ => scala.None
      }
    }
  }
}
