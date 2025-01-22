package io.scalaland.chimney.internal.compiletime.dsl

import io.scalaland.chimney.Patcher
import io.scalaland.chimney.internal.*
import io.scalaland.chimney.dsl.*
import io.scalaland.chimney.internal.compiletime.derivation.patcher.PatcherMacros
import io.scalaland.chimney.internal.compiletime.dsl.utils.DslMacroUtils
import io.scalaland.chimney.internal.runtime.{PatcherFlags, PatcherOverrides, Path, WithRuntimeDataStore}
import io.scalaland.chimney.internal.runtime.PatcherOverrides.*

import scala.quoted.*

object PatcherDefinitionMacros {

  def withFieldConstImpl[
      A: Type,
      Patch: Type,
      Overrides <: PatcherOverrides: Type,
      Flags <: PatcherFlags: Type,
      T: Type,
      U: Type
  ](
      pu: Expr[PatcherDefinition[A, Patch, Overrides, Flags]],
      selectorObj: Expr[A => T],
      value: Expr[U]
  )(using Quotes): Expr[PatcherDefinition[A, Patch, ? <: PatcherOverrides, Flags]] =
    DslMacroUtils().applyFieldNameType {
      [objPath <: Path] =>
        (_: Type[objPath]) ?=>
          '{
            WithRuntimeDataStore
              .update($pu, $value)
              .asInstanceOf[PatcherDefinition[A, Patch, Const[objPath, Overrides], Flags]]
        }
    }(selectorObj)

  def withFieldComputedImpl[
      A: Type,
      Patch: Type,
      Overrides <: PatcherOverrides: Type,
      Flags <: PatcherFlags: Type,
      T: Type,
      U: Type
  ](
      pu: Expr[PatcherDefinition[A, Patch, Overrides, Flags]],
      selectorObj: Expr[A => T],
      f: Expr[Patch => U]
  )(using Quotes): Expr[PatcherDefinition[A, Patch, ? <: PatcherOverrides, Flags]] =
    DslMacroUtils().applyFieldNameType {
      [objPath <: Path] =>
        (_: Type[objPath]) ?=>
          '{
            WithRuntimeDataStore
              .update($pu, $f)
              .asInstanceOf[PatcherDefinition[A, Patch, Computed[Path.Root, objPath, Overrides], Flags]]
        }
    }(selectorObj)

  def withFieldComputedFromImpl[
      A: Type,
      Patch: Type,
      Overrides <: PatcherOverrides: Type,
      Flags <: PatcherFlags: Type,
      S: Type,
      T: Type,
      U: Type
  ](
      pu: Expr[PatcherDefinition[A, Patch, Overrides, Flags]],
      selectorPatch: Expr[Patch => S],
      selectorObj: Expr[A => T],
      f: Expr[S => U]
  )(using Quotes): Expr[PatcherDefinition[A, Patch, ? <: PatcherOverrides, Flags]] =
    DslMacroUtils().applyFieldNameTypes {
      [patchPath <: Path, objPath <: Path] =>
        (_: Type[patchPath]) ?=>
          (_: Type[objPath]) ?=>
            '{
              WithRuntimeDataStore
                .update($pu, $f)
                .asInstanceOf[PatcherDefinition[A, Patch, Computed[patchPath, objPath, Overrides], Flags]]
          }
    }(selectorPatch, selectorObj)

  def withFieldIgnoredImpl[
      A: Type,
      Patch: Type,
      Overrides <: PatcherOverrides: Type,
      Flags <: PatcherFlags: Type,
      T: Type
  ](
      pu: Expr[PatcherDefinition[A, Patch, Overrides, Flags]],
      selectorPatch: Expr[Patch => T]
  )(using Quotes): Expr[PatcherDefinition[A, Patch, ? <: PatcherOverrides, Flags]] =
    DslMacroUtils().applyFieldNameType {
      [patchPath <: Path] =>
        (_: Type[patchPath]) ?=>
          '{
            $pu.asInstanceOf[PatcherDefinition[A, Patch, Ignored[patchPath, Overrides], Flags]]
        }
    }(selectorPatch)
}
