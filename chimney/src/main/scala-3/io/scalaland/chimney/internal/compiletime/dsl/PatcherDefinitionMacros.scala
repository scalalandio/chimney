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
      pu: Expr[PatcherUsing[A, Patch, Overrides, Flags]],
      selectorObj: Expr[A => T],
      value: Expr[U]
  )(using Quotes): Expr[PatcherUsing[A, Patch, ? <: PatcherOverrides, Flags]] =
    DslMacroUtils().applyFieldNameType {
      [objPath <: Path] =>
        (_: Type[objPath]) ?=>
          '{
            WithRuntimeDataStore
              .update($pu, $value)
              .asInstanceOf[PatcherUsing[A, Patch, Const[objPath, Overrides], Flags]]
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
      pu: Expr[PatcherUsing[A, Patch, Overrides, Flags]],
      selectorObj: Expr[A => T],
      f: Expr[Patch => U]
  )(using Quotes): Expr[PatcherUsing[A, Patch, ? <: PatcherOverrides, Flags]] =
    DslMacroUtils().applyFieldNameType {
      [objPath <: Path] =>
        (_: Type[objPath]) ?=>
          '{
            WithRuntimeDataStore
              .update($pu, $f)
              .asInstanceOf[PatcherUsing[A, Patch, Computed[Path.Root, objPath, Overrides], Flags]]
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
      pu: Expr[PatcherUsing[A, Patch, Overrides, Flags]],
      selectorPatch: Expr[Patch => T],
      selectorObj: Expr[A => S],
      f: Expr[S => U]
  )(using Quotes): Expr[PatcherUsing[A, Patch, ? <: PatcherOverrides, Flags]] =
    DslMacroUtils().applyFieldNameTypes {
      [patchPath <: Path, objPath <: Path] =>
        (_: Type[patchPath]) ?=>
          (_: Type[objPath]) ?=>
            '{
              WithRuntimeDataStore
                .update($pu, $f)
                .asInstanceOf[PatcherUsing[A, Patch, Computed[patchPath, objPath, Overrides], Flags]]
          }
    }(selectorPatch, selectorObj)

  def withFieldUnusedImpl[
      A: Type,
      Patch: Type,
      Overrides <: PatcherOverrides: Type,
      Flags <: PatcherFlags: Type,
      T: Type
  ](
      pu: Expr[PatcherUsing[A, Patch, Overrides, Flags]],
      selectorObj: Expr[A => T]
  )(using Quotes): Expr[PatcherUsing[A, Patch, ? <: PatcherOverrides, Flags]] =
    DslMacroUtils().applyFieldNameType {
      [objPath <: Path] =>
        (_: Type[objPath]) ?=>
          '{
            $pu.asInstanceOf[PatcherUsing[A, Patch, Ignored[objPath, Overrides], Flags]]
        }
    }(selectorObj)
}
