package io.scalaland.chimney.internal.compiletime.derivation

import io.scalaland.chimney.dsl.TransformerDefinitionCommons
import io.scalaland.chimney.{PartialTransformer, Patcher, Transformer}
import io.scalaland.chimney.partial
import io.scalaland.chimney.internal.compiletime.Definitions

import scala.annotation.nowarn

@nowarn("msg=The outer reference in this type test cannot be checked at run time.")
private[compiletime] trait Contexts { this: Definitions & Configurations =>

  sealed protected trait TransformationContext[From, To] extends Product with Serializable {
    val src: Expr[From]

    val From: Type[From]
    val To: Type[To]

    val runtimeDataStore: Expr[TransformerDefinitionCommons.RuntimeDataStore]
    val config: TransformerConfig
    val derivationStartedAt: java.time.Instant

    type Target
    val Target: Type[Target]
    type TypeClass
    val TypeClass: Type[TypeClass]

    def updateFromTo[NewFrom: Type, NewTo: Type](newSrc: Expr[NewFrom]): TransformationContext[NewFrom, NewTo] =
      fold[TransformationContext[NewFrom, NewTo]] { (ctx: TransformationContext.ForTotal[From, To]) =>
        TransformationContext.ForTotal[NewFrom, NewTo](src = newSrc)(
          From = Type[NewFrom],
          To = Type[NewTo],
          runtimeDataStore = ctx.runtimeDataStore,
          config = ctx.config,
          ctx.derivationStartedAt
        )
      } { (ctx: TransformationContext.ForPartial[From, To]) =>
        TransformationContext.ForPartial[NewFrom, NewTo](src = newSrc, failFast = ctx.failFast)(
          From = Type[NewFrom],
          To = Type[NewTo],
          runtimeDataStore = ctx.runtimeDataStore,
          config = ctx.config,
          ctx.derivationStartedAt
        )
      }

    def updateConfig(update: TransformerConfig => TransformerConfig): TransformationContext[From, To] =
      fold[TransformationContext[From, To]] { (ctx: TransformationContext.ForTotal[From, To]) =>
        TransformationContext.ForTotal[From, To](src = ctx.src)(
          From = ctx.From,
          To = ctx.To,
          runtimeDataStore = ctx.runtimeDataStore,
          config = update(ctx.config),
          derivationStartedAt = ctx.derivationStartedAt
        )
      } { (ctx: TransformationContext.ForPartial[From, To]) =>
        TransformationContext.ForPartial[From, To](src = ctx.src, failFast = ctx.failFast)(
          From = ctx.From,
          To = ctx.To,
          runtimeDataStore = ctx.runtimeDataStore,
          config = update(ctx.config),
          derivationStartedAt = ctx.derivationStartedAt
        )
      }

    /** Avoid clumsy
     * {{{
     * @nowarn("msg=The outer reference in this type test cannot be checked at run time.")
     * ctx match {
     *   case total: TransformationContext.ForTotal[?, ?]     => ...
     *   case partial: TransformationContext.ForPartial[?, ?] => ...
     * }
     * }}}
     */
    def fold[B](
        forTotal: TransformationContext.ForTotal[From, To] => B
    )(
        forPartial: TransformationContext.ForPartial[From, To] => B
    ): B
  }
  protected object TransformationContext {

    final case class ForTotal[From, To](src: Expr[From])(
        val From: Type[From],
        val To: Type[To],
        val runtimeDataStore: Expr[TransformerDefinitionCommons.RuntimeDataStore],
        val config: TransformerConfig,
        val derivationStartedAt: java.time.Instant
    ) extends TransformationContext[From, To] {

      final type Target = To
      val Target = To
      final type TypeClass = Transformer[From, To]
      val TypeClass = ChimneyType.Transformer(From, To)

      override def fold[B](
          forTotal: TransformationContext.ForTotal[From, To] => B
      )(
          forPartial: TransformationContext.ForPartial[From, To] => B
      ): B = forTotal(this)

      override def toString: String =
        s"ForTotal[From = ${Type.prettyPrint(From)}, To = ${Type.prettyPrint(To)}](src = ${Expr.prettyPrint(src)})($config)"
    }
    object ForTotal {

      def create[From: Type, To: Type](
          src: Expr[From],
          config: TransformerConfig,
          runtimeDataStore: Expr[TransformerDefinitionCommons.RuntimeDataStore]
      ): ForTotal[From, To] =
        ForTotal(src = src)(
          From = Type[From],
          To = Type[To],
          runtimeDataStore = runtimeDataStore,
          config = config.withDefinitionScope(Type[From].asComputed -> Type[To].asComputed),
          derivationStartedAt = java.time.Instant.now()
        )
    }

    final case class ForPartial[From, To](src: Expr[From], failFast: Expr[Boolean])(
        val From: Type[From],
        val To: Type[To],
        val runtimeDataStore: Expr[TransformerDefinitionCommons.RuntimeDataStore],
        val config: TransformerConfig,
        val derivationStartedAt: java.time.Instant
    ) extends TransformationContext[From, To] {

      final type Target = partial.Result[To]
      val Target = ChimneyType.PartialResult(To)
      final type TypeClass = PartialTransformer[From, To]
      val TypeClass = ChimneyType.PartialTransformer(From, To)

      override def fold[B](
          forTotal: TransformationContext.ForTotal[From, To] => B
      )(
          forPartial: TransformationContext.ForPartial[From, To] => B
      ): B = forPartial(this)

      override def toString: String =
        s"ForPartial[From = ${Type.prettyPrint(From)}, To = ${Type
            .prettyPrint(To)}](src = ${Expr.prettyPrint(src)}, failFast = ${Expr.prettyPrint(failFast)})($config)"
    }
    object ForPartial {

      def create[From: Type, To: Type](
          src: Expr[From],
          failFast: Expr[Boolean],
          config: TransformerConfig,
          runtimeDataStore: Expr[TransformerDefinitionCommons.RuntimeDataStore]
      ): ForPartial[From, To] = ForPartial(src = src, failFast = failFast)(
        From = Type[From],
        To = Type[To],
        runtimeDataStore = runtimeDataStore,
        config = config.withDefinitionScope(Type[From].asComputed -> Type[To].asComputed),
        derivationStartedAt = java.time.Instant.now()
      )
    }
  }

  final case class PatcherContext[A, Patch](obj: Expr[A], patch: Expr[Patch])(
      val A: Type[A],
      val Patch: Type[Patch]
  ) {

    final type Target = A
    val Target = A
    final type TypeClass = Patcher[A, Patch]
    val TypeClass = ChimneyType.Patcher(A, Patch)
  }
  object PatcherContext {

    def create[A: Type, Patch: Type](obj: Expr[A], patch: Expr[Patch]): PatcherContext[A, Patch] =
      PatcherContext(obj = obj, patch = patch)(
        A = Type[A],
        Patch = Type[Patch]
      )
  }

  // unpacks Types from Contexts
  implicit final protected def ctx2FromType[From, To](implicit ctx: TransformationContext[From, To]): Type[From] =
    ctx.From
  implicit final protected def ctx2ToType[From, To](implicit ctx: TransformationContext[From, To]): Type[To] = ctx.To
  implicit final protected def ctx2TType[A, Patch](implicit ctx: PatcherContext[A, Patch]): Type[A] = ctx.A
  implicit final protected def ctx2PatchType[A, Patch](implicit ctx: PatcherContext[A, Patch]): Type[Patch] = ctx.Patch

  // for unpacking Exprs from Context, import ctx.* should be enough
}
