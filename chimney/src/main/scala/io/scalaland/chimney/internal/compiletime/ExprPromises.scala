package io.scalaland.chimney.internal.compiletime

import scala.annotation.nowarn

@nowarn("msg=The outer reference in this type test cannot be checked at run time.")
trait ExprPromises { this: Definitions =>

  type ExprPromiseName

  final protected class ExprPromise[From: Type, A](usage: A, fromName: ExprPromiseName) {

    def map[B](f: A => B): ExprPromise[From, B] = new ExprPromise(f(usage), fromName)

    def traverse[G[_]: fp.Applicative, B](f: A => G[B]): G[ExprPromise[From, B]] = {
      import fp.Syntax.*
      f(usage).map(new ExprPromise(_, fromName))
    }

    def fulfilAsVal[From2: Type](init: Expr[From2]): DerivationResult[PrependValsTo[A]] =
      if (Type[From2] <:< Type[From])
        DerivationResult.pure(
          new PrependValsTo(usage, Vector(fromName -> ComputedExpr(Expr.asInstanceOf[From2, From](init))))
        )
      else
        DerivationResult.fromException(
          new AssertionError(
            s"Initialized deferred Expr[${Type.prettyPrint[From]}] with expression of type ${Type.prettyPrint[From2]}"
          )
        )

    def fulfilAsLambda[To: Type, B](f: Expr[From => To] => B)(implicit ev: A <:< Expr[To]): B =
      ExprPromise.createAndUseLambda(fromName, ev(usage), f)

    def foldEither[L, R, B](
        left: ExprPromise[From, L] => B
    )(
        right: ExprPromise[From, R] => B
    )(implicit
        ev: A <:< Either[L, R]
    ): B = ev(usage) match {
      case Left(value)  => left(new ExprPromise(value, fromName))
      case Right(value) => right(new ExprPromise(value, fromName))
    }
  }
  protected val ExprPromise: ExprPromiseModule
  protected trait ExprPromiseModule { this: ExprPromise.type =>

    final def promise[From: Type](nameGenerationStrategy: NameGenerationStrategy): ExprPromise[From, Expr[From]] = {
      val name = provideFreshName[From](nameGenerationStrategy)
      new ExprPromise(createRefToName[From](name), name)
    }

    protected def provideFreshName[From: Type](nameGenerationStrategy: NameGenerationStrategy): ExprPromiseName
    protected def createRefToName[From: Type](name: ExprPromiseName): Expr[From]
    def createAndUseLambda[From: Type, To: Type, B](
        fromName: ExprPromiseName,
        to: Expr[To],
        usage: Expr[From => To] => B
    ): B

    sealed trait NameGenerationStrategy extends Product with Serializable
    object NameGenerationStrategy {
      final case class FromPrefix(src: String) extends NameGenerationStrategy
      case object FromType extends NameGenerationStrategy
      final case class FromExpr[A](expr: Expr[A]) extends NameGenerationStrategy
    }
  }

  implicit def ExprPromiseTraverse[From]: fp.Traverse[ExprPromise[From, *]] = new fp.Traverse[ExprPromise[From, *]] {

    def traverse[G[_]: fp.Applicative, A, B](fa: ExprPromise[From, A])(f: A => G[B]): G[ExprPromise[From, B]] =
      fa.traverse(f)
  }

  final protected class PrependValsTo[A](
      private val usage: A,
      private val vals: Vector[(ExprPromiseName, ComputedExpr)]
  ) {

    def map[B](f: A => B): PrependValsTo[B] = new PrependValsTo(f(usage), vals)

    def map2[B, C](val2: PrependValsTo[B])(f: (A, B) => C): PrependValsTo[C] =
      new PrependValsTo(f(usage, val2.usage), vals ++ val2.vals)

    def traverse[G[_]: fp.Applicative, B](f: A => G[B]): G[PrependValsTo[B]] = {
      import fp.Syntax.*
      f(usage).map(new PrependValsTo(_, vals))
    }

    def prepend[B](implicit ev: A <:< Expr[B]): Expr[B] = PrependValsTo.initializeVals(vals, ev(usage))
  }
  protected val PrependValsTo: PrependValsToModule
  protected trait PrependValsToModule { this: PrependValsTo.type =>

    def initializeVals[To](vals: Vector[(ExprPromiseName, ComputedExpr)], expr: Expr[To]): Expr[To]
  }

  implicit val PrependValsToTraversableApplicative: fp.ApplicativeTraverse[PrependValsTo] =
    new fp.ApplicativeTraverse[PrependValsTo] {

      def map2[A, B, C](fa: PrependValsTo[A], fb: PrependValsTo[B])(f: (A, B) => C): PrependValsTo[C] = fa.map2(fb)(f)

      def pure[A](a: A): PrependValsTo[A] = new PrependValsTo[A](a, Vector.empty)

      def traverse[G[_]: fp.Applicative, A, B](fa: PrependValsTo[A])(f: A => G[B]): G[PrependValsTo[B]] = fa.traverse(f)
    }
}
