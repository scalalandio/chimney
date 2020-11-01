package io.scalaland.chimney.internal.macros.dsl

import io.scalaland.chimney
import io.scalaland.chimney.{TransformerF, TransformerFSupport}
import io.scalaland.chimney.internal.macros.TransformerMacros
import io.scalaland.chimney.internal.utils.{DerivationGuards, EitherUtils, MacroUtils}

import scala.reflect.macros.blackbox

class TransformerBlackboxMacros(val c: blackbox.Context)
    extends TransformerMacros
    with DerivationGuards
    with MacroUtils
    with EitherUtils {

  import c.universe._

  def buildTransformerImpl[
      From: WeakTypeTag,
      To: WeakTypeTag,
      C: WeakTypeTag,
      Flags: WeakTypeTag,
      ScopeFlags: WeakTypeTag
  ](tc: c.Tree): c.Expr[chimney.Transformer[From, To]] = {
    c.Expr[chimney.Transformer[From, To]](buildDefinedTransformer[From, To, C, Flags, ScopeFlags]())
  }

  def buildTransformerFImpl[
      F[+_]: TypeConstructorTag,
      From: WeakTypeTag,
      To: WeakTypeTag,
      C: WeakTypeTag,
      InstanceFlags: WeakTypeTag,
      ScopeFlags: WeakTypeTag
  ](
      tfs: c.Expr[TransformerFSupport[F]],
      tc: c.Tree
  ): c.Expr[TransformerF[F, From, To]] = {
    c.Expr[TransformerF[F, From, To]](
      buildDefinedTransformer[From, To, C, InstanceFlags, ScopeFlags](tfs.tree, Some(TypeConstructorTag[F]))
    )
  }

  def transformImpl[
      From: WeakTypeTag,
      To: WeakTypeTag,
      C: WeakTypeTag,
      InstanceFlags: WeakTypeTag,
      ScopeFlags: WeakTypeTag
  ](tc: c.Tree): c.Expr[To] = {
    c.Expr[To](expandTransform[From, To, C, InstanceFlags, ScopeFlags](tc))
  }

  def transformFImpl[
      F[+_]: TypeConstructorTag,
      From: WeakTypeTag,
      To: WeakTypeTag,
      C: WeakTypeTag,
      InstanceFlags: WeakTypeTag,
      ScopeFlags: WeakTypeTag
  ](
      tc: c.Tree,
      tfs: c.Expr[TransformerFSupport[F]]
  ): c.Expr[F[To]] = {
    c.Expr[F[To]](expandTransform[From, To, C, InstanceFlags, ScopeFlags](tc, tfs.tree, Some(TypeConstructorTag[F])))
  }

  def deriveTransformerImpl[From: WeakTypeTag, To: WeakTypeTag]: c.Expr[chimney.Transformer[From, To]] = {
    val tcTree = findLocalTransformerConfigurationFlags
    val flags = captureFromTransformerConfigurationTree(tcTree)

    val transformerTree = genTransformer[From, To](
      TransformerConfig(
        flags = flags,
        definitionScope = Some((weakTypeOf[From], weakTypeOf[To]))
      )
    )

    c.Expr[chimney.Transformer[From, To]] {
      q"""{
        val _ = $tcTree // hack to avoid unused warnings
        $transformerTree
      }"""
    }
  }

  def deriveTransformerFImpl[F[+_]: TypeConstructorTag, From: WeakTypeTag, To: WeakTypeTag](
      tfs: c.Expr[TransformerFSupport[F]]
  ): c.Expr[TransformerF[F, From, To]] = {

    val tcTree = findLocalTransformerConfigurationFlags
    val flags = captureFromTransformerConfigurationTree(tcTree)
    val wrapperType = Some(TypeConstructorTag[F])

    val transformerTree = genTransformer[From, To](
      TransformerConfig(
        flags = flags,
        definitionScope = Some((weakTypeOf[From], weakTypeOf[To])),
        wrapperType = wrapperType,
        wrapperSupportInstance = tfs.tree,
        wrapperErrorPathSupportInstance = findTransformerErrorPathSupport(wrapperType)
      )
    )

    c.Expr[chimney.TransformerF[F, From, To]] {
      q"""{
        val _ = $tcTree // hack to avoid unused warnings
        $transformerTree
      }"""
    }
  }
}
