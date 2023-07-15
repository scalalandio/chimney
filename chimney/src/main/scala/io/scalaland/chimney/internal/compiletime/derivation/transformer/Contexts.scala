package io.scalaland.chimney.internal.compiletime.derivation.transformer

import io.scalaland.chimney.dsl.TransformerDefinitionCommons
import io.scalaland.chimney.partial

private[compiletime] trait Contexts { this: Derivation =>

  sealed protected trait TransformationContext[From, To] extends scala.Product with Serializable {
    val src: Expr[From]

    val From: Type[From]
    val To: Type[To]

    val runtimeDataStore: Expr[TransformerDefinitionCommons.RuntimeDataStore]
    val config: TransformerConfig
    val derivationStartedAt: java.time.Instant

    type Target
    val Target: Type[Target]

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

    def updateConfig(update: TransformerConfig => TransformerConfig): this.type =
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
      }.asInstanceOf[this.type]

    /** Avoid clumsy
     * {{{
     *
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
          config = config.withDefinitionScope(Type[From].as_?? -> Type[To].as_??),
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
        config = config.withDefinitionScope(Type[From].as_?? -> Type[To].as_??),
        derivationStartedAt = java.time.Instant.now()
      )
    }
  }

  // unpacks Types from Contexts
  implicit final protected def ctx2FromType[From, To](implicit ctx: TransformationContext[From, To]): Type[From] =
    ctx.From
  implicit final protected def ctx2ToType[From, To](implicit ctx: TransformationContext[From, To]): Type[To] = ctx.To

  // for unpacking Exprs from Context, pattern matching should be enough
}
