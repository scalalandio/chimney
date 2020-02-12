package io.scalaland.chimney.internal.macros

import io.scalaland.chimney.internal.utils.{DerivationGuards, EitherUtils, MacroUtils}
import io.scalaland.chimney.{Id, Patcher, TransformerF}

import scala.reflect.macros.blackbox

class ChimneyBlackboxMacros(val c: blackbox.Context)
    extends PatcherMacros
    with TransformerMacros
    with DerivationGuards
    with MacroUtils
    with EitherUtils {

  def buildTransformerImpl[F[_]: WTTF, From: c.WeakTypeTag, To: c.WeakTypeTag, C: c.WeakTypeTag]
      : c.Expr[TransformerF[F, From, To]] = {
    c.Expr[TransformerF[F, From, To]](buildDefinedTransformer[F, From, To, C])
  }

  def transformImpl[F[_]: WTTF, From: c.WeakTypeTag, To: c.WeakTypeTag, C: c.WeakTypeTag]: c.Expr[F[To]] = {
    c.Expr[F[To]](expandTransform[F, From, To, C])
  }

  def deriveTransformerImpl[F[_]: WTTF, From: c.WeakTypeTag, To: c.WeakTypeTag]: c.Expr[TransformerF[F, From, To]] = {
    import c.universe._
    genTransformer[F, From, To](TransformerConfig(definitionScope = Some((weakTypeOf[From], weakTypeOf[To]))))
  }

  def deriveTransformerImplId[From: c.WeakTypeTag, To: c.WeakTypeTag]: c.Expr[TransformerF[Id, From, To]] = {
    deriveTransformerImpl[Id, From, To]
  }

  def patchImpl[T: c.WeakTypeTag, Patch: c.WeakTypeTag, C: c.WeakTypeTag]: c.Expr[T] = {
    c.Expr[T](expandPatch[T, Patch, C])
  }

  def derivePatcherImpl[T: c.WeakTypeTag, Patch: c.WeakTypeTag]: c.Expr[Patcher[T, Patch]] = {
    genPatcher[T, Patch](PatcherConfig())
  }
}
