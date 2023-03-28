package io.scalaland.chimney.internal.compiletime

import io.scalaland.chimney.{PartialTransformer, Patcher, Transformer}
import io.scalaland.chimney.partial

import scala.annotation.nowarn

@nowarn("msg=The outer reference in this type test cannot be checked at run time.")
private[compiletime] trait Contexts { this: Definitions & Configurations =>

  /** Lazily evaluated log entry */
  final protected class LogEntry(val nesting: Int, thunk: => String) {
    lazy val message: String = thunk
  }

  sealed protected trait Context {

    type Target
    type Typeclass

    def logs: Vector[LogEntry]
    def appendLog(msg: => String): this.type
  }

  protected object Context {
    sealed trait ForTransformer[From, To] extends Context {
      val From: Type[From]
      val To: Type[To]
      val src: Expr[From]

      val config: TransformerConfig[From, To]
    }

    final case class ForTotal[From, To](
        From: Type[From],
        To: Type[To],
        src: Expr[From],
        config: TransformerConfig[From, To],
        logs: Vector[LogEntry],
        logNesting: Int
    ) extends ForTransformer[From, To] {

      final type Target = To
      final type Typeclass = Transformer[From, To]

      override def appendLog(msg: => String): this.type =
        copy(logs = logs :+ new LogEntry(nesting = logNesting, thunk = msg)).asInstanceOf[this.type]
    }
    object ForTotal {

      def create[From: Type, To: Type](src: Expr[From], config: TransformerConfig[From, To]): ForTotal[From, To] =
        ForTotal(
          From = implicitly[Type[From]],
          To = implicitly[Type[To]],
          src = src,
          config = config,
          logs = Vector.empty,
          logNesting = 0
        )
    }

    final case class ForPartial[From, To](
        From: Type[From],
        To: Type[To],
        src: Expr[From],
        failFast: Expr[Boolean],
        config: TransformerConfig[From, To],
        logs: Vector[LogEntry],
        logNesting: Int
    ) extends ForTransformer[From, To] {

      final type Target = partial.Result[To]
      final type Typeclass = PartialTransformer[From, To]

      override def appendLog(msg: => String): this.type =
        copy(logs = logs :+ new LogEntry(nesting = logNesting, thunk = msg)).asInstanceOf[this.type]
    }
    object ForPartial {

      def create[From: Type, To: Type](
          src: Expr[From],
          failFast: Expr[Boolean],
          config: TransformerConfig[From, To]
      ): ForPartial[From, To] = ForPartial(
        From = implicitly[Type[From]],
        To = implicitly[Type[To]],
        src = src,
        failFast = failFast,
        config = config,
        logs = Vector.empty,
        logNesting = 0
      )
    }

    final case class ForPatcher[T, Patch](
        T: Type[T],
        Patch: Type[Patch],
        obj: Expr[T],
        patch: Expr[Patch],
        logs: Vector[LogEntry],
        logNesting: Int
    ) extends Context {

      final type Target = T
      final type Typeclass = Patcher[T, Patch]

      override def appendLog(msg: => String): this.type =
        copy(logs = logs :+ new LogEntry(nesting = logNesting, thunk = msg)).asInstanceOf[this.type]
    }
    object ForPatcher {

      def create[T: Type, Patch: Type](obj: Expr[T], patch: Expr[Patch]): ForPatcher[T, Patch] = ForPatcher(
        T = implicitly[Type[T]],
        Patch = implicitly[Type[Patch]],
        obj = obj,
        patch = patch,
        logs = Vector.empty,
        logNesting = 0
      )
    }
  }

  import Context.*

  // unpacks Types from Contexts
  implicit final protected def ctx2FromType[From, To](implicit ctx: ForTransformer[From, To]): Type[From] = ctx.From
  implicit final protected def ctx2ToType[From, To](implicit ctx: ForTransformer[From, To]): Type[To] = ctx.To
  implicit final protected def ctx2TType[T, Patch](implicit ctx: ForPatcher[T, Patch]): Type[T] = ctx.T
  implicit final protected def ctx2PatchType[T, Patch](implicit ctx: ForPatcher[T, Patch]): Type[Patch] = ctx.Patch

  // for unpacking Exprs from Context, import ctx.* should be enogh
}
