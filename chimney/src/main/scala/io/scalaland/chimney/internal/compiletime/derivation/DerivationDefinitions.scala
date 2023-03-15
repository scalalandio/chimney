package io.scalaland.chimney.internal.compiletime.derivation

import io.scalaland.chimney.internal.compiletime.Definitions
import io.scalaland.chimney.{partial, PartialTransformer, Transformer}

import scala.annotation.nowarn

@nowarn("msg=The outer reference in this type test cannot be checked at run time.")
private[derivation] trait DerivationDefinitions { this: Definitions =>

  protected type Id[A] = A

  // TODO: sth like that to replave Inputs or whatever we call it

  sealed protected trait DerivedExpr[T, P] extends Product with Serializable {
    type From

    def isTotal: Boolean
    def isPartial: Boolean

    def From: Type[From]
    def src: Expr[From]

    def transformExpr[T2, P2](
        transformTotal: (Type[From], Expr[From], Expr[T]) => DerivationResult[Expr[T2]]
    )(
        transformPartial: (Type[From], Expr[From], Expr[Boolean], Expr[P]) => DerivationResult[Expr[P2]]
    ): DerivationResult[DerivedExpr[T2, P2]]

    def toEither: Either[Expr[T], Expr[P]]
  }
  protected object DerivedExpr {

    def emptyTotal[From: Type](
        src: Expr[From]
    ): DerivedExpr[Unit, Unit] = DerivedTotalExpr(Type[From], src, Expr.Unit)

    def emptyPartial[From: Type](
        src: Expr[From],
        failFast: Expr[Boolean]
    ): DerivedExpr[Unit, Unit] = DerivedPartialExpr(Type[From], src, failFast, Expr.Unit)

    final case class DerivedTotalExpr[From0, T, P](
        From: Type[From0],
        src: Expr[From0],
        expr: Expr[T]
    ) extends DerivedExpr[T, P] {

      type From = From0
      def isTotal = true
      def isPartial = false

      override def transformExpr[T2, P2](
          transformTotal: (Type[From0], Expr[From0], Expr[T]) => DerivationResult[Expr[T2]]
      )(
          transformPartial: (Type[From0], Expr[From0], Expr[Boolean], Expr[P]) => DerivationResult[Expr[P2]]
      ): DerivationResult[DerivedExpr[T2, P2]] = transformTotal(From, src, expr).map(expr2 => copy(expr = expr2))

      override def toEither: Either[Expr[T], Expr[P]] = Left(expr)
    }

    final case class DerivedPartialExpr[From0, T, P](
        From: Type[From0],
        src: Expr[From0],
        failFast: Expr[Boolean],
        expr: Expr[P]
    ) extends DerivedExpr[T, P] {

      type From = From0
      def isTotal = false
      def isPartial = true

      override def transformExpr[T2, P2](
          transformTotal: (Type[From0], Expr[From0], Expr[T]) => DerivationResult[Expr[T2]]
      )(
          transformPartial: (Type[From0], Expr[From0], Expr[Boolean], Expr[P]) => DerivationResult[Expr[P2]]
      ): DerivationResult[DerivedExpr[T2, P2]] =
        transformPartial(From, src, failFast, expr).map(expr2 => copy(expr = expr2))

      override def toEither: Either[Expr[T], Expr[P]] = Right(expr)
    }
  }

  protected def instantiateTotalTransformer[From: Type, To: Type](
      f: Expr[From] => DerivationResult[Expr[To]]
  ): DerivationResult[Expr[Transformer[From, To]]]

  protected def instantiatePartialTransformer[From: Type, To: Type](
      f: (Expr[From], Expr[Boolean]) => DerivationResult[Expr[partial.Result[To]]]
  ): DerivationResult[Expr[PartialTransformer[From, To]]]
}
