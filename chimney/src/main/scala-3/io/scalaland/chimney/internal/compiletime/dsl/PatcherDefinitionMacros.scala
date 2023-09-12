package io.scalaland.chimney.internal.compiletime.dsl

import io.scalaland.chimney.Patcher
import io.scalaland.chimney.dsl.*
import io.scalaland.chimney.internal.compiletime.derivation.patcher.PatcherMacros
import io.scalaland.chimney.internal.runtime.{PatcherCfg, PatcherFlags}

import scala.quoted.*

object PatcherDefinitionMacros {

  def buildPatcher[
      A: Type,
      Patch: Type,
      Cfg <: PatcherCfg: Type,
      Flags <: PatcherFlags: Type,
      ImplicitScopeFlags <: PatcherFlags: Type
  ](using Quotes): Expr[Patcher[A, Patch]] =
    PatcherMacros.derivePatcherWithConfig[A, Patch, Cfg, Flags, ImplicitScopeFlags]
}
