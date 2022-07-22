package io.scalaland.chimney.internal.macros.dsl

import io.scalaland.chimney.Patcher
import io.scalaland.chimney.internal.macros.{PatcherMacros, TransformerMacros}

import scala.reflect.macros.blackbox

class PatcherBlackboxMacros(val c: blackbox.Context) extends PatcherMacros with TransformerMacros {

  import c.universe._

  def patchImpl[T: WeakTypeTag, Patch: WeakTypeTag, C: WeakTypeTag]: c.Expr[T] = {
    c.Expr[T](expandPatch[T, Patch, C])
  }

  def derivePatcherImpl[T: WeakTypeTag, Patch: WeakTypeTag]: c.Expr[Patcher[T, Patch]] = {
    genPatcher[T, Patch](PatcherConfig())
  }
}
