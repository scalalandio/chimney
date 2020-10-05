package io.scalaland.chimney.internal.macros

import io.scalaland.chimney
import io.scalaland.chimney.internal.utils.{DerivationGuards, EitherUtils, MacroUtils}
import io.scalaland.chimney.{Patcher, TransformerF, TransformerFSupport}

import scala.reflect.macros.blackbox

class ChimneyBlackboxMacros(val c: blackbox.Context)
    extends PatcherMacros
    with TransformerMacros
    with DerivationGuards
    with MacroUtils
    with EitherUtils {

  import c.universe._

  def buildTransformerImpl[From: WeakTypeTag, To: WeakTypeTag, C: WeakTypeTag, Flags: WeakTypeTag]
      : c.Expr[chimney.Transformer[From, To]] = {
    c.Expr[chimney.Transformer[From, To]](buildDefinedTransformer[From, To, C, Flags]())
  }

  def buildTransformerFImpl[F[+_], From: WeakTypeTag, To: WeakTypeTag, C: WeakTypeTag, Flags: WeakTypeTag](
      tfs: c.Expr[TransformerFSupport[F]]
  ): c.Expr[TransformerF[F, From, To]] = {
    c.Expr[TransformerF[F, From, To]](buildDefinedTransformer[From, To, C, Flags](tfs.tree))
  }

  def transformImpl[From: WeakTypeTag, To: WeakTypeTag, C: WeakTypeTag, Flags: WeakTypeTag]: c.Expr[To] = {
    c.Expr[To](expandTransform[From, To, C, Flags]())
  }

  def transformFImpl[F[+_], From: WeakTypeTag, To: WeakTypeTag, C: WeakTypeTag, Flags: WeakTypeTag](
      tfs: c.Expr[TransformerFSupport[F]]
  ): c.Expr[F[To]] = {
    c.Expr[F[To]](expandTransform[From, To, C, Flags](tfs.tree))
  }

  def deriveTransformerImpl[From: WeakTypeTag, To: WeakTypeTag]: c.Expr[chimney.Transformer[From, To]] = {
    c.Expr[chimney.Transformer[From, To]](
      genTransformer[From, To](
        TransformerConfig(
          definitionScope = Some((weakTypeOf[From], weakTypeOf[To]))
        )
      )
    )
  }

  def deriveTransformerFImpl[F[+_], From: WeakTypeTag, To: WeakTypeTag](
      tfs: c.Expr[TransformerFSupport[F]]
  )(
      implicit F: WeakTypeTag[F[_]]
  ): c.Expr[TransformerF[F, From, To]] = {
    c.Expr[TransformerF[F, From, To]](
      genTransformer[From, To](
        TransformerConfig(
          definitionScope = Some((weakTypeOf[From], weakTypeOf[To])),
          wrapperType = Some(F.tpe),
          wrapperSupportInstance = tfs.tree
        )
      )
    )
  }

  def patchImpl[T: WeakTypeTag, Patch: WeakTypeTag, C: WeakTypeTag]: c.Expr[T] = {
    c.Expr[T](expandPatch[T, Patch, C])
  }

  def derivePatcherImpl[T: WeakTypeTag, Patch: WeakTypeTag]: c.Expr[Patcher[T, Patch]] = {
    genPatcher[T, Patch](PatcherConfig())
  }
}
