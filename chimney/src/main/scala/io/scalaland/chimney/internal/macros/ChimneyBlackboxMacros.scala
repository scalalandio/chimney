package io.scalaland.chimney.internal.macros

import io.scalaland.chimney.internal.utils.{DerivationGuards, EitherUtils, MacroUtils}
import io.scalaland.chimney.{Patcher, Transformer, TransformerF, TransformerFSupport}

import scala.reflect.macros.blackbox

class ChimneyBlackboxMacros(val c: blackbox.Context)
    extends PatcherMacros
    with TransformerMacros
    with DerivationGuards
    with MacroUtils
    with EitherUtils {

  def buildTransformerImpl[From: c.WeakTypeTag, To: c.WeakTypeTag, C: c.WeakTypeTag]: c.Expr[Transformer[From, To]] = {
    c.Expr[Transformer[From, To]](buildDefinedTransformer[From, To, C]())
  }

  def buildTransformerFImpl[F[+_], From: c.WeakTypeTag, To: c.WeakTypeTag, C: c.WeakTypeTag](
      tfs: c.Expr[TransformerFSupport[F]]
  ): c.Expr[TransformerF[F, From, To]] = {
    c.Expr[TransformerF[F, From, To]](buildDefinedTransformer[From, To, C](tfs.tree))
  }

  def transformImpl[From: c.WeakTypeTag, To: c.WeakTypeTag, C: c.WeakTypeTag]: c.Expr[To] = {
    c.Expr[To](expandTransform[From, To, C]())
  }

  def transformFImpl[F[+_], From: c.WeakTypeTag, To: c.WeakTypeTag, C: c.WeakTypeTag](
      tfs: c.Expr[TransformerFSupport[F]]
  ): c.Expr[F[To]] = {
    c.Expr[F[To]](expandTransform[From, To, C](tfs.tree))
  }

  def deriveTransformerImpl[From: c.WeakTypeTag, To: c.WeakTypeTag]: c.Expr[Transformer[From, To]] = {
    c.Expr[Transformer[From, To]](
      genTransformer[From, To](
        TransformerConfig(
          definitionScope = Some((c.weakTypeOf[From], c.weakTypeOf[To]))
        )
      )
    )
  }

  def deriveTransformerFImpl[F[+_], From: c.WeakTypeTag, To: c.WeakTypeTag](
      tfs: c.Expr[TransformerFSupport[F]]
  )(
      implicit F: c.WeakTypeTag[F[_]]
  ): c.Expr[TransformerF[F, From, To]] = {
    c.Expr[TransformerF[F, From, To]](
      genTransformer[From, To](
        TransformerConfig(
          definitionScope = Some((c.weakTypeOf[From], c.weakTypeOf[To])),
          wrapperType = Some(F.tpe),
          wrapperSupportInstance = tfs.tree
        )
      )
    )
  }

  def patchImpl[T: c.WeakTypeTag, Patch: c.WeakTypeTag, C: c.WeakTypeTag]: c.Expr[T] = {
    c.Expr[T](expandPatch[T, Patch, C])
  }

  def derivePatcherImpl[T: c.WeakTypeTag, Patch: c.WeakTypeTag]: c.Expr[Patcher[T, Patch]] = {
    genPatcher[T, Patch](PatcherConfig())
  }
}
