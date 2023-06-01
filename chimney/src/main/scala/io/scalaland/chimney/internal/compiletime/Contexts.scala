package io.scalaland.chimney.internal.compiletime

import io.scalaland.chimney.dsl.TransformerDefinitionCommons
import io.scalaland.chimney.{PartialTransformer, Patcher, Transformer}
import io.scalaland.chimney.partial

import scala.annotation.nowarn

@nowarn("msg=The outer reference in this type test cannot be checked at run time.")
private[compiletime] trait Contexts { this: Definitions & Configurations =>

  // TODO: rename to TransformationContext
  sealed protected trait TransformerContext[From, To] extends Product with Serializable {
    val From: Type[From]
    val To: Type[To]
    val src: Expr[From]

    val config: TransformerConfig

    type Target
    val Target: Type[Target]
    type TypeClass
    val TypeClass: Type[TypeClass]

    val derivationStartedAt: java.time.Instant

    def updateFromTo[NewFrom: Type, NewTo: Type](newSrc: Expr[NewFrom]): TransformerContext[NewFrom, NewTo] =
      this match {
        case total: TransformerContext.ForTotal[?, ?] =>
          total.copy(From = Type[NewFrom], To = Type[NewTo], src = newSrc)
        case partial: TransformerContext.ForPartial[?, ?] =>
          partial.copy(From = Type[NewFrom], To = Type[NewTo], src = newSrc)
      }

    def updateConfig(f: TransformerConfig => TransformerConfig): TransformerContext[From, To] = this match {
      case total: TransformerContext.ForTotal[?, ?]     => total.copy(config = f(total.config))
      case partial: TransformerContext.ForPartial[?, ?] => partial.copy(config = f(partial.config))
    }
  }
  protected object TransformerContext {

    final case class ForTotal[From, To](
        From: Type[From],
        To: Type[To],
        src: Expr[From],
        runtimeDataStore: Expr[TransformerDefinitionCommons.RuntimeDataStore],
        config: TransformerConfig,
        derivationStartedAt: java.time.Instant
    ) extends TransformerContext[From, To] {

      final type Target = To
      val Target = To
      final type TypeClass = Transformer[From, To]
      val TypeClass = ChimneyType.Transformer(From, To)

      override def toString: String =
        s"Total(From = ${Type.prettyPrint(From)}, To = ${Type.prettyPrint(To)}, src = ${Expr.prettyPrint(src)}, $config)"
    }
    object ForTotal {

      def create[From: Type, To: Type](
          src: Expr[From],
          config: TransformerConfig,
          runtimeDataStore: Expr[TransformerDefinitionCommons.RuntimeDataStore]
      ): ForTotal[From, To] =
        ForTotal(
          From = Type[From],
          To = Type[To],
          src = src,
          runtimeDataStore = runtimeDataStore,
          config = config.withDefinitionScope((ComputedType(Type[From]), ComputedType(Type[To]))),
          derivationStartedAt = java.time.Instant.now()
        )
    }

    final case class ForPartial[From, To](
        From: Type[From],
        To: Type[To],
        src: Expr[From],
        failFast: Expr[Boolean],
        runtimeDataStore: Expr[TransformerDefinitionCommons.RuntimeDataStore],
        config: TransformerConfig,
        derivationStartedAt: java.time.Instant
    ) extends TransformerContext[From, To] {

      final type Target = partial.Result[To]
      val Target = ChimneyType.PartialResult(To)
      final type TypeClass = PartialTransformer[From, To]
      val TypeClass = ChimneyType.PartialTransformer(From, To)

      override def toString: String =
        s"Partial(From = ${Type.prettyPrint(From)}, To = ${Type.prettyPrint(To)}, src = ${Expr
            .prettyPrint(src)}, failFast = ${Expr.prettyPrint(failFast)}, $config)"
    }
    object ForPartial {

      def create[From: Type, To: Type](
          src: Expr[From],
          failFast: Expr[Boolean],
          config: TransformerConfig,
          runtimeDataStore: Expr[TransformerDefinitionCommons.RuntimeDataStore]
      ): ForPartial[From, To] = ForPartial(
        From = Type[From],
        To = Type[To],
        src = src,
        failFast = failFast,
        runtimeDataStore = runtimeDataStore,
        config = config.withDefinitionScope((ComputedType(Type[From]), ComputedType(Type[To]))),
        derivationStartedAt = java.time.Instant.now()
      )
    }
  }

  final case class PatcherContext[A, Patch](
      A: Type[A],
      Patch: Type[Patch],
      obj: Expr[A],
      patch: Expr[Patch]
  ) {

    final type Target = A
    val Target = A
    final type TypeClass = Patcher[A, Patch]
    val TypeClass = ChimneyType.Patcher(A, Patch)
  }
  object PatcherContext {

    def create[A: Type, Patch: Type](obj: Expr[A], patch: Expr[Patch]): PatcherContext[A, Patch] = PatcherContext(
      A = Type[A],
      Patch = Type[Patch],
      obj = obj,
      patch = patch
    )
  }

  // unpacks Types from Contexts
  implicit final protected def ctx2FromType[From, To](implicit ctx: TransformerContext[From, To]): Type[From] = ctx.From
  implicit final protected def ctx2ToType[From, To](implicit ctx: TransformerContext[From, To]): Type[To] = ctx.To
  implicit final protected def ctx2TType[A, Patch](implicit ctx: PatcherContext[A, Patch]): Type[A] = ctx.A
  implicit final protected def ctx2PatchType[A, Patch](implicit ctx: PatcherContext[A, Patch]): Type[Patch] = ctx.Patch

  // for unpacking Exprs from Context, import ctx.* should be enough
}
