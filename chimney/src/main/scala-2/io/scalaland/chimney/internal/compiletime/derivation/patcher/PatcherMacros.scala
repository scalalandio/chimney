package io.scalaland.chimney.internal.compiletime.derivation.patcher

import io.scalaland.chimney.{dsl, Patcher}
import io.scalaland.chimney.internal.runtime

import scala.reflect.macros.blackbox

final class PatcherMacros(val c: blackbox.Context) extends DerivationPlatform with Gateway {

  import c.universe.{internal as _, Transformer as _, *}

  def derivePatchImpl[A: WeakTypeTag, Patch: WeakTypeTag, Cfg <: runtime.PatcherCfg: WeakTypeTag]: c.Expr[A] =
    cacheDefinition(c.Expr[dsl.PatcherUsing[A, Patch, Cfg]](c.prefix.tree)) { pu =>
      Expr.block(
        List(Expr.suppressUnused(pu)),
        derivePatcherResult(obj = c.Expr[A](q"$pu.obj"), patch = c.Expr[Patch](q"$pu.objPatch"))
      )
    }

  def derivePatcherImpl[A: WeakTypeTag, Patch: WeakTypeTag]: c.Expr[Patcher[A, Patch]] =
    derivePatcher[A, Patch]
}
