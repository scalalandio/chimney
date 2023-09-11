package io.scalaland.chimney.internal.compiletime.derivation.patcher

import io.scalaland.chimney.Patcher
import io.scalaland.chimney.internal.runtime

import scala.quoted.{Expr, Quotes, Type}

final class PatcherMacros(q: Quotes) extends DerivationPlatform(q) with Gateway

object PatcherMacros {

  final def derivePatcherResult[
      A: Type,
      Patch: Type,
      Cfg <: runtime.PatcherCfg: Type,
      Flags <: runtime.PatcherFlags: Type,
      ImplicitScopeFlags <: runtime.PatcherFlags
  ](
      obj: Expr[A],
      patch: Expr[Patch]
  )(using q: Quotes): Expr[A] = new PatcherMacros(q).derivePatcherResult[A, Patch, Flags](obj, patch)

  final def derivePatcher[A: Type, Patch: Type](using q: Quotes): Expr[Patcher[A, Patch]] =
    new PatcherMacros(q).derivePatcher[A, Patch]
}
