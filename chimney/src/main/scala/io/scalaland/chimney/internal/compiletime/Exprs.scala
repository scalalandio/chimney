package io.scalaland.chimney.internal.compiletime

private[compiletime] trait Exprs { this: Definitions =>

  /** Platform-specific expression representation (c.universe.Expr[A] in 2, quotes.Expr[A] in 3 */
  protected type Expr[A]
  protected val Expr: ExprModule
  protected trait ExprModule { this: Expr.type =>

    val Nothing: Expr[Nothing]
    val Unit: Expr[Unit]

    object Function1 {
      def instance[A: Type, B: Type](f: Expr[A] => Expr[B]): Expr[A => B] =
        ExprPromise.promise[A](ExprPromise.NameGenerationStrategy.FromType).map[Expr[B]](f).fulfilAsLambda
    }

    val Function2: Function2Module
    trait Function2Module {
      this: Function2.type =>
      def instance[A: Type, B: Type, C: Type](f: (Expr[A], Expr[B]) => Expr[C]): Expr[(A, B) => C] =
        ExprPromise
          .promise[A](ExprPromise.NameGenerationStrategy.FromType)
          .fulfilAsLambda2[B, Expr[B], C](
            ExprPromise.promise[B](ExprPromise.NameGenerationStrategy.FromType)
          )(f)

      def tupled[A: Type, B: Type, C: Type](fn2: Expr[(A, B) => C]): Expr[((A, B)) => C]
    }

    val Array: ArrayModule
    trait ArrayModule { this: Array.type =>
      def apply[A: Type](args: Expr[A]*): Expr[Array[A]]

      def map[A: Type, B: Type](array: Expr[Array[A]])(fExpr: Expr[A => B]): Expr[Array[B]]

      def to[A: Type, C: Type](array: Expr[Array[A]])(
          factoryExpr: Expr[scala.collection.compat.Factory[A, C]]
      ): Expr[C]

      def iterator[A: Type](array: Expr[Array[A]]): Expr[Iterator[A]]
    }

    val Option: OptionModule
    trait OptionModule { this: Option.type =>
      def apply[A: Type](a: Expr[A]): Expr[Option[A]]
      def empty[A: Type]: Expr[Option[A]]
      val None: Expr[scala.None.type]
      def map[A: Type, B: Type](opt: Expr[Option[A]])(f: Expr[A => B]): Expr[Option[B]]
      def fold[A: Type, B: Type](opt: Expr[Option[A]])(none: Expr[B])(f: Expr[A => B]): Expr[B]
      def getOrElse[A: Type](opt: Expr[Option[A]])(orElse: Expr[A]): Expr[A]
    }

    val Either: EitherModule
    trait EitherModule { this: Either.type =>
      def fold[L: Type, R: Type, A: Type](either: Expr[Either[L, R]])(left: Expr[L => A])(right: Expr[R => A]): Expr[A]

      val Left: LeftModule
      trait LeftModule { this: Left.type =>
        def apply[L: Type, R: Type](value: Expr[L]): Expr[Left[L, R]]

        def value[L: Type, R: Type](left: Expr[Left[L, R]]): Expr[L]
      }
      val Right: RightModule
      trait RightModule { this: Right.type =>
        def apply[L: Type, R: Type](value: Expr[R]): Expr[Right[L, R]]

        def value[L: Type, R: Type](right: Expr[Right[L, R]]): Expr[R]
      }
    }

    val Iterable: IterableModule
    trait IterableModule { this: Iterable.type =>
      def map[A: Type, B: Type](iterable: Expr[Iterable[A]])(fExpr: Expr[A => B]): Expr[Iterable[B]]

      def to[A: Type, C: Type](iterable: Expr[Iterable[A]])(
          factoryExpr: Expr[scala.collection.compat.Factory[A, C]]
      ): Expr[C]

      def iterator[A: Type](iterable: Expr[Iterable[A]]): Expr[Iterator[A]]
    }

    val Map: MapModule
    trait MapModule { this: Map.type =>
      def iterator[K: Type, V: Type](map: Expr[Map[K, V]]): Expr[Iterator[(K, V)]]
    }

    val Iterator: IteratorModule
    trait IteratorModule { this: Iterator.type =>
      def map[A: Type, B: Type](iterator: Expr[Iterator[A]])(fExpr: Expr[A => B]): Expr[Iterator[B]]

      def to[A: Type, C: Type](iterator: Expr[Iterator[A]])(
          factoryExpr: Expr[scala.collection.compat.Factory[A, C]]
      ): Expr[C]

      def zipWithIndex[A: Type](it: Expr[Iterator[A]]): Expr[Iterator[(A, Int)]]
    }

    def summonImplicit[A: Type]: Option[Expr[A]]

    def asInstanceOf[A: Type, B: Type](expr: Expr[A]): Expr[B]

    def upcast[A: Type, B: Type](expr: Expr[A]): Expr[B]

    def prettyPrint[A](expr: Expr[A]): String

    def typeOf[A](expr: Expr[A]): Type[A]
  }
  implicit final protected class ExprOps[A: Type](private val expr: Expr[A]) {

    def prettyPrint: String = Expr.prettyPrint(expr)

    def tpe: Type[A] = Expr.typeOf(expr)

    // All of methods below change Expr[A] to Expr[B], but they differ in checks ans how it affects the underlying code:
    // - asInstanceOfExpr should be used when we want to generate .asInstanceOf in generated code, because we need to
    //   perform the check in the runtime
    // - widenExpr should be used when we e.g. have List[A] and we want to use .map method from Iterable to create
    //   List[B] but without loosing information about concrete type in the generated code, because we proved ourselves
    //   that the generated code matches our expectations, but it would be PITA to juggle F[_] around
    // - upcastExpr should be used in simple cases when we can get away with just doing '{ a : B } to access methods
    //   we have defined for super type without juggling type constructors around

    /** Creates '{ ${ expr }.asInstanceOf[B] } expression in emitted code, moving check to the runtime */
    def asInstanceOfExpr[B: Type]: Expr[B] = Expr.asInstanceOf[A, B](expr)

    /** Upcasts Expr[A] to Expr[B] if A <:< B, without upcasting the underlying code */
    def widenExpr[B: Type]: Expr[B] = {
      Predef.assert(
        Type[A] <:< Type[B],
        s"Upcasting can only be done to type proved to be super type! Failed ${Type.prettyPrint[A]} <:< ${Type.prettyPrint[B]} check"
      )
      expr.asInstanceOf[Expr[B]]
    }

    /** Upcasts Expr[A] to Expr[B] in the emitted code: '{ (${ expr }) : B } */
    def upcastExpr[B: Type]: Expr[B] = Expr.upcast[A, B](expr)
  }

  implicit final protected class Function1[A: Type, B: Type](private val function1Expr: Expr[A => B]) {

    def apply(a: Expr[A]): Expr[B] = ??? // TODO!!!
  }

  implicit final protected class Function2[A: Type, B: Type, C: Type](private val function2Expr: Expr[(A, B) => C]) {

    def tupled: Expr[((A, B)) => C] = Expr.Function2.tupled(function2Expr)
  }

  implicit final protected class ArrayExprOps[A: Type](private val arrayExpr: Expr[Array[A]]) {

    def map[B: Type](fExpr: Expr[A => B]): Expr[Array[B]] = Expr.Array.map(arrayExpr)(fExpr)
    def to[C: Type](factoryExpr: Expr[scala.collection.compat.Factory[A, C]]): Expr[C] =
      Expr.Array.to(arrayExpr)(factoryExpr)
    def iterator: Expr[Iterator[A]] = Expr.Array.iterator(arrayExpr)
  }

  implicit final protected class OptionExprOps[A: Type](private val optionExpr: Expr[Option[A]]) {

    def map[B: Type](fExpr: Expr[A => B]): Expr[Option[B]] = Expr.Option.map(optionExpr)(fExpr)
    def fold[B: Type](noneExpr: Expr[B])(fExpr: Expr[A => B]): Expr[B] =
      Expr.Option.fold(optionExpr)(noneExpr)(fExpr)
    def getOrElse(noneExpr: Expr[A]): Expr[A] = Expr.Option.getOrElse(optionExpr)(noneExpr)
  }

  implicit final protected class EitherExprOps[L: Type, R: Type](private val eitherExpr: Expr[Either[L, R]]) {

    def fold[B: Type](onLeft: Expr[L => B])(onRight: Expr[R => B]): Expr[B] =
      Expr.Either.fold(eitherExpr)(onLeft)(onRight)
  }

  implicit final protected class LeftExprOps[L: Type, R: Type](private val leftExpr: Expr[Left[L, R]]) {

    def value: Expr[L] = Expr.Either.Left.value(leftExpr)
  }

  implicit final protected class RightExprOps[L: Type, R: Type](private val rightExpr: Expr[Right[L, R]]) {

    def value: Expr[R] = Expr.Either.Right.value(rightExpr)
  }

  implicit final protected class IterableExprOps[A: Type](private val iterableExpr: Expr[Iterable[A]]) {

    def map[B: Type](fExpr: Expr[A => B]): Expr[Iterable[B]] = Expr.Iterable.map(iterableExpr)(fExpr)
    def to[C: Type](factoryExpr: Expr[scala.collection.compat.Factory[A, C]]): Expr[C] =
      Expr.Iterable.to(iterableExpr)(factoryExpr)
    def iterator: Expr[Iterator[A]] = Expr.Iterable.iterator(iterableExpr)
  }

  implicit final protected class MapExprOps[K: Type, V: Type](private val mapExpr: Expr[Map[K, V]]) {

    def iterator: Expr[Iterator[(K, V)]] = Expr.Map.iterator(mapExpr)
  }

  implicit final protected class IteratorExprOps[A: Type](private val iteratorExpr: Expr[Iterator[A]]) {

    def map[B: Type](fExpr: Expr[A => B]): Expr[Iterator[B]] = Expr.Iterator.map(iteratorExpr)(fExpr)
    def to[C: Type](factoryExpr: Expr[scala.collection.compat.Factory[A, C]]): Expr[C] =
      Expr.Iterator.to(iteratorExpr)(factoryExpr)
    def zipWithIndex: Expr[Iterator[(A, Int)]] = Expr.Iterator.zipWithIndex(iteratorExpr)
  }
}
