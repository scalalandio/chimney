package io.scalaland.chimney.internal.compiletime

import io.scalaland.chimney.{PartialTransformer, Patcher, Transformer}
import io.scalaland.chimney.partial

import scala.annotation.nowarn

@nowarn("msg=The outer reference in this type test cannot be checked at run time.")
private[compiletime] trait Contexts { this: Definitions & Configurations =>

  sealed protected trait TransformerContext[From, To] extends Product with Serializable {
    val From: Type[From]
    val To: Type[To]
    val src: Expr[From]

    val config: TransformerConfig

    type Target
    val Target: Type[Target]
    type TypeClass
    val TypeClass: Type[TypeClass]
  }
  protected object TransformerContext {

    final case class ForTotal[From, To](
        From: Type[From],
        To: Type[To],
        src: Expr[From],
        config: TransformerConfig
    ) extends TransformerContext[From, To] {

      final type Target = To
      val Target = To
      final type TypeClass = Transformer[From, To]
      val TypeClass = ChimneyType.Transformer(From, To)
    }
    object ForTotal {

      def create[From: Type, To: Type](src: Expr[From], config: TransformerConfig): ForTotal[From, To] =
        ForTotal(
          From = Type[From],
          To = Type[To],
          src = src,
          config = config
        )
    }

    final case class ForPartial[From, To](
        From: Type[From],
        To: Type[To],
        src: Expr[From],
        failFast: Expr[Boolean],
        config: TransformerConfig
    ) extends TransformerContext[From, To] {

      final type Target = partial.Result[To]
      val Target = ChimneyType.PartialResult(To)
      final type TypeClass = PartialTransformer[From, To]
      val TypeClass = ChimneyType.PartialTransformer(From, To)
    }
    object ForPartial {

      def create[From: Type, To: Type](
          src: Expr[From],
          failFast: Expr[Boolean],
          config: TransformerConfig
      ): ForPartial[From, To] = ForPartial(
        From = Type[From],
        To = Type[To],
        src = src,
        failFast = failFast,
        config = config
      )
    }
  }

  final case class PatcherContext[T, Patch](
      T: Type[T],
      Patch: Type[Patch],
      obj: Expr[T],
      patch: Expr[Patch]
  ) {

    final type Target = T
    val Target = T
    final type TypeClass = Patcher[T, Patch]
    val TypeClass = ChimneyType.Patcher(T, Patch)
  }
  object PatcherContext {

    def create[T: Type, Patch: Type](obj: Expr[T], patch: Expr[Patch]): PatcherContext[T, Patch] = PatcherContext(
      T = Type[T],
      Patch = Type[Patch],
      obj = obj,
      patch = patch
    )
  }

  // unpacks Types from Contexts
  implicit final protected def ctx2FromType[From, To](implicit ctx: TransformerContext[From, To]): Type[From] = ctx.From
  implicit final protected def ctx2ToType[From, To](implicit ctx: TransformerContext[From, To]): Type[To] = ctx.To
  implicit final protected def ctx2TType[T, Patch](implicit ctx: PatcherContext[T, Patch]): Type[T] = ctx.T
  implicit final protected def ctx2PatchType[T, Patch](implicit ctx: PatcherContext[T, Patch]): Type[Patch] = ctx.Patch

  // for unpacking Exprs from Context, import ctx.* should be enough
}
