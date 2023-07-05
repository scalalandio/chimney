package io.scalaland.chimney.internal.compiletime.derivation.patcher

import io.scalaland.chimney.{dsl, Patcher}
import io.scalaland.chimney.internal.PatcherCfg

import scala.reflect.macros.blackbox

final class PatcherMacros(val c: blackbox.Context) extends DerivationPlatform with Gateway {

  import c.universe.{internal as _, Transformer as _, *}

  def derivePatchImpl[T: WeakTypeTag, Patch: WeakTypeTag, C <: PatcherCfg: WeakTypeTag]: c.Expr[T] = {
    cacheDefinition(c.Expr[dsl.PatcherUsing[T, Patch, C]](c.prefix.tree)) { pu =>
      Expr.block(
        List(Expr.suppressUnused(pu)),
        derivePatcherResult(obj = c.Expr[T](q"$pu.obj"), patch = c.Expr[Patch](q"$pu.objPatch"))
      )
    }
  }

  def derivePatcherImpl[T: WeakTypeTag, Patch: WeakTypeTag]: c.Expr[Patcher[T, Patch]] = {
    derivePatcher[T, Patch]
  }

}
