package io.scalaland.chimney.internal.compiletime.dsl

import io.scalaland.chimney.Patcher
import io.scalaland.chimney.dsl.*
import io.scalaland.chimney.internal.compiletime.derivation.patcher.PatcherMacros
import io.scalaland.chimney.internal.runtime.{PatcherFlags, PatcherOverrides}

import scala.quoted.*

object PatcherDefinitionMacros {

  def buildPatcher[
      A: Type,
      Patch: Type,
      Tail <: PatcherOverrides: Type,
      Flags <: PatcherFlags: Type,
      ImplicitScopeFlags <: PatcherFlags: Type
  ](using Quotes): Expr[Patcher[A, Patch]] =
    PatcherMacros.derivePatcherWithConfig[A, Patch, Tail, Flags, ImplicitScopeFlags]
}
