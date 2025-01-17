package io.scalaland.chimney.internal.compiletime

private[compiletime] trait Exprs { this: Definitions =>

  /** Platform-specific expression representation (`c.Expr[A]` in 2, `scala.quoted.Expr[A]` in 3 */
  protected type Expr[A]
  protected val Expr: ExprModule
  protected trait ExprModule { this: Expr.type =>

    // Build-in types expressions

    val Nothing: Expr[Nothing]
    val Null: Expr[Null]
    val Unit: Expr[Unit]

    // FIXME (2.0.0 cleanup): this should be following the pattern: apply, unapply and [A <: Type]: Expr[A]
    def Boolean(value: Boolean): Expr[Boolean]
    def Int(value: Int): Expr[Int]
    def Long(value: Long): Expr[Long]
    def Float(value: Float): Expr[Float]
    def Double(value: Double): Expr[Double]
    def Char(value: Char): Expr[Char]
    def String(value: String): Expr[String]

    def Tuple2[A: Type, B: Type](a: Expr[A], b: Expr[B]): Expr[(A, B)]

    val Function1: Function1Module
    trait Function1Module { this: Function1.type =>
      def apply[A: Type, B: Type](fn: Expr[A => B])(a: Expr[A]): Expr[B]

      def instance[A: Type, B: Type](f: Expr[A] => Expr[B]): Expr[A => B] =
        ExprPromise.promise[A](ExprPromise.NameGenerationStrategy.FromType).map[Expr[B]](f).fulfilAsLambda
    }

    val Function2: Function2Module
    trait Function2Module { this: Function2.type =>
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
      def fold[A: Type, B: Type](opt: Expr[Option[A]])(onNone: Expr[B])(onSome: Expr[A => B]): Expr[B]
      def orElse[A: Type](opt1: Expr[Option[A]], opt2: Expr[Option[A]]): Expr[Option[A]]
      def getOrElse[A: Type](opt: Expr[Option[A]])(orElse: Expr[A]): Expr[A]
      def get[A: Type](opt: Expr[Option[A]]): Expr[A]
      def isDefined[A: Type](opt: Expr[Option[A]]): Expr[Boolean]
    }

    val Either: EitherModule
    trait EitherModule { this: Either.type =>
      def fold[L: Type, R: Type, A: Type](either: Expr[Either[L, R]])(left: Expr[L => A])(right: Expr[R => A]): Expr[A]

      def orElse[L: Type, R: Type](either1: Expr[Either[L, R]], either2: Expr[Either[L, R]]): Expr[Either[L, R]]

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
      def iterator[K: Type, V: Type](map: Expr[scala.collection.Map[K, V]]): Expr[Iterator[(K, V)]]
    }

    val Iterator: IteratorModule
    trait IteratorModule { this: Iterator.type =>
      def map[A: Type, B: Type](iterator: Expr[Iterator[A]])(fExpr: Expr[A => B]): Expr[Iterator[B]]

      def concat[A: Type](iterator: Expr[Iterator[A]], iterator2: Expr[Iterator[A]]): Expr[Iterator[A]]

      def to[A: Type, C: Type](iterator: Expr[Iterator[A]])(
          factoryExpr: Expr[scala.collection.compat.Factory[A, C]]
      ): Expr[C]

      def zipWithIndex[A: Type](it: Expr[Iterator[A]]): Expr[Iterator[(A, Int)]]
    }

    def ifElse[A: Type](cond: Expr[Boolean])(ifBranch: Expr[A])(elseBranch: Expr[A]): Expr[A]

    def block[A: Type](statements: List[Expr[Unit]], expr: Expr[A]): Expr[A]

    def summonImplicit[A: Type]: Option[Expr[A]]
    def summonImplicitUnsafe[A: Type]: Expr[A] = summonImplicit[A].getOrElse {
      // $COVERAGE-OFF$should never happen unless we messed up
      assertionFailed(s"Implicit not found: ${Type.prettyPrint[A]}")
      // $COVERAGE-ON$
    }

    def nowarn[A: Type](warnings: Option[String])(expr: Expr[A]): Expr[A]
    def SuppressWarnings[A: Type](warnings: List[String])(expr: Expr[A]): Expr[A]

    def suppressUnused[A: Type](expr: Expr[A]): Expr[Unit]

    // Implementations of Expr extension methods

    def eq[A: Type, B: Type](a: Expr[A], b: Expr[B]): Expr[Boolean]

    def asInstanceOf[A: Type, B: Type](expr: Expr[A]): Expr[B]

    def upcast[A: Type, B: Type](expr: Expr[A]): Expr[B]

    def prettyPrint[A](expr: Expr[A]): String

    def typeOf[A](expr: Expr[A]): Type[A]
  }
  implicit final protected class ExprOps[A: Type](private val expr: Expr[A]) {

    def prettyPrint: String = Expr.prettyPrint(expr)

    def tpe: Type[A] = Expr.typeOf(expr)

    /** Creates '{ $expr == $other } expression, which would compare both expressions in runtime */
    def eqExpr[B: Type](other: Expr[B]): Expr[Boolean] = Expr.eq(expr, other)

    // Both methods below change Expr[A] to Expr[B], but they differ in checks and how it affects the underlying code:
    // - asInstanceOfExpr[B] should be used when we want to have .asInstanceOf[B] in the generated code, because we need
    //   to perform the cast in the runtime - WE know what we can perform it but the JVN does not
    // - upcastToExprOf[B] should be used when WE know that A <: B, but it is not obvious to Scala compiler - in such
    //   case Type[A] <:< Type[B] assertion will be checked and the expression upcasted

    /** Creates '{ ${ expr }.asInstanceOf[B] } expression in emitted code, moving check to the runtime */
    def asInstanceOfExpr[B: Type]: Expr[B] = Expr.asInstanceOf[A, B](expr)

    /** Upcasts `Expr[A]` to `Expr[B]` if `A <:< B`, without upcasting the underlying code */
    def upcastToExprOf[B: Type]: Expr[B] = Expr.upcast[A, B](expr)

    def as_?? : ExistentialExpr = ExistentialExpr(expr)
  }

  implicit final protected class Function1[A: Type, B: Type](private val function1Expr: Expr[A => B]) {

    def apply(a: Expr[A]): Expr[B] = Expr.Function1.apply(function1Expr)(a)
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

    def isDefined: Expr[Boolean] = Expr.Option.isDefined(optionExpr)
    def map[B: Type](fExpr: Expr[A => B]): Expr[Option[B]] = Expr.Option.map(optionExpr)(fExpr)
    def fold[B: Type](noneExpr: Expr[B])(fExpr: Expr[A => B]): Expr[B] =
      Expr.Option.fold(optionExpr)(noneExpr)(fExpr)
    def getOrElse(noneExpr: Expr[A]): Expr[A] = Expr.Option.getOrElse(optionExpr)(noneExpr)
    def get: Expr[A] = Expr.Option.get(optionExpr)
    def orElse(other: Expr[Option[A]]): Expr[Option[A]] = Expr.Option.orElse(optionExpr, other)
  }

  implicit final protected class EitherExprOps[L: Type, R: Type](private val eitherExpr: Expr[Either[L, R]]) {

    def fold[B: Type](matchingLeft: Expr[L => B])(matchingRight: Expr[R => B]): Expr[B] =
      Expr.Either.fold(eitherExpr)(matchingLeft)(matchingRight)
    def orElse(either2Expr: Expr[Either[L, R]]): Expr[Either[L, R]] =
      Expr.Either.orElse(eitherExpr, either2Expr)
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

  implicit final protected class MapExprOps[K: Type, V: Type](private val mapExpr: Expr[scala.collection.Map[K, V]]) {

    def iterator: Expr[Iterator[(K, V)]] = Expr.Map.iterator(mapExpr)
  }

  implicit final protected class IteratorExprOps[A: Type](private val iteratorExpr: Expr[Iterator[A]]) {

    def map[B: Type](fExpr: Expr[A => B]): Expr[Iterator[B]] = Expr.Iterator.map(iteratorExpr)(fExpr)
    def concat(iteratorExpr2: Expr[Iterator[A]]): Expr[Iterator[A]] = Expr.Iterator.concat(iteratorExpr, iteratorExpr2)
    def to[C: Type](factoryExpr: Expr[scala.collection.compat.Factory[A, C]]): Expr[C] =
      Expr.Iterator.to(iteratorExpr)(factoryExpr)
    def zipWithIndex: Expr[Iterator[(A, Int)]] = Expr.Iterator.zipWithIndex(iteratorExpr)
  }
}
