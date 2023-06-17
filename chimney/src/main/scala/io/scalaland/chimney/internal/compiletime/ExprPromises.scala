package io.scalaland.chimney.internal.compiletime

private[compiletime] trait ExprPromises { this: Definitions =>

  protected type ExprPromiseName

  final protected class ExprPromise[From: Type, A](private val usage: A, private val fromName: ExprPromiseName) {

    def map[B](f: A => B): ExprPromise[From, B] = new ExprPromise(f(usage), fromName)

    def traverse[G[_]: fp.Applicative, B](f: A => G[B]): G[ExprPromise[From, B]] = {
      import fp.Syntax.*
      f(usage).map(new ExprPromise(_, fromName))
    }

    private def fulfilAsDefinition[From2: Type](
        init: Expr[From2],
        definitionType: PrependValsTo.DefnType
    ): DerivationResult[PrependDefinitionsTo[A]] =
      if (Type[From2] <:< Type[From])
        DerivationResult.pure(
          new PrependDefinitionsTo(
            usage,
            Vector((fromName, ExistentialExpr(Expr.asInstanceOf[From2, From](init)), definitionType))
          )
        )
      else
        DerivationResult.assertionError(
          s"Initialized deferred Expr[${Type.prettyPrint[From]}] with expression of type ${Type.prettyPrint[From2]}"
        )

    def fulfilAsDef[From2: Type](init: Expr[From2]): DerivationResult[PrependDefinitionsTo[A]] =
      fulfilAsDefinition(init, PrependValsTo.DefnType.Def)
    def fulfilAsLazy[From2: Type](init: Expr[From2]): DerivationResult[PrependDefinitionsTo[A]] =
      fulfilAsDefinition(init, PrependValsTo.DefnType.Lazy)
    def fulfilAsVal[From2: Type](init: Expr[From2]): DerivationResult[PrependDefinitionsTo[A]] =
      fulfilAsDefinition(init, PrependValsTo.DefnType.Val)
    def fulfilAsVar[From2: Type](
        init: Expr[From2]
    ): DerivationResult[PrependDefinitionsTo[(A, Expr[From] => Expr[Unit])]] =
      fulfilAsDefinition(init, PrependValsTo.DefnType.Var)
        .map(_.map(_ -> PrependValsTo.setVal(fromName)))

    private def fulfilAsLambdaIn[To: Type, B](use: Expr[From => To] => B)(implicit ev: A <:< Expr[To]): B =
      ExprPromise.createAndUseLambda(fromName, ev(usage), use)
    def fulfilAsLambda[To: Type](implicit ev: A <:< Expr[To]): Expr[From => To] =
      fulfilAsLambdaIn[To, Expr[From => To]](identity)

    private def fulfilAsLambda2In[From2: Type, B, To: Type, C](
        promise: ExprPromise[From2, B]
    )(
        combine: (A, B) => Expr[To]
    )(
        use: Expr[(From, From2) => To] => C
    ): C =
      ExprPromise.createAndUseLambda2(fromName, promise.fromName, combine(usage, promise.usage), use)

    def fulfilAsLambda2[From2: Type, B, To: Type](
        promise: ExprPromise[From2, B]
    )(
        combine: (A, B) => Expr[To]
    ): Expr[(From, From2) => To] =
      fulfilAsLambda2In[From2, B, To, Expr[(From, From2) => To]](promise)(combine)(identity)

    def fulfillAsPatternMatchCase[To](implicit ev: A <:< Expr[To]): PatternMatchCase[To] =
      new PatternMatchCase(someFrom = ExistentialType(Type[From]), usage = ev(usage), fromName = fromName)

    def partition[L, R](implicit
        ev: A <:< Either[L, R]
    ): Either[ExprPromise[From, L], ExprPromise[From, R]] = ev(usage) match {
      case Left(value)  => Left(new ExprPromise(value, fromName))
      case Right(value) => Right(new ExprPromise(value, fromName))
    }

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

    def isLeft[L, R](implicit ev: A <:< Either[L, R]): Boolean = foldEither[L, R, Boolean](_ => true)(_ => false)
    def isRight[L, R](implicit ev: A <:< Either[L, R]): Boolean = foldEither[L, R, Boolean](_ => false)(_ => true)
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
    def createAndUseLambda2[From: Type, From2: Type, To: Type, B](
        fromName: ExprPromiseName,
        from2Name: ExprPromiseName,
        to: Expr[To],
        usage: Expr[(From, From2) => To] => B
    ): B

    sealed trait NameGenerationStrategy extends Product with Serializable
    object NameGenerationStrategy {
      final case class FromPrefix(src: String) extends NameGenerationStrategy
      case object FromType extends NameGenerationStrategy
      final case class FromExpr[A](expr: Expr[A]) extends NameGenerationStrategy
    }
  }

  implicit protected def ExprPromiseTraverse[From]: fp.Traverse[ExprPromise[From, *]] =
    new fp.Traverse[ExprPromise[From, *]] {

      def traverse[G[_]: fp.Applicative, A, B](fa: ExprPromise[From, A])(f: A => G[B]): G[ExprPromise[From, B]] =
        fa.traverse(f)
    }

  final protected class PrependDefinitionsTo[A](
      private val usage: A,
      private val defns: Vector[(ExprPromiseName, ExistentialExpr, PrependValsTo.DefnType)]
  ) {

    def map[B](f: A => B): PrependDefinitionsTo[B] = new PrependDefinitionsTo(f(usage), defns)

    def map2[B, C](val2: PrependDefinitionsTo[B])(f: (A, B) => C): PrependDefinitionsTo[C] =
      new PrependDefinitionsTo(f(usage, val2.usage), defns ++ val2.defns)

    def traverse[G[_]: fp.Applicative, B](f: A => G[B]): G[PrependDefinitionsTo[B]] = {
      import fp.Syntax.*
      f(usage).map(new PrependDefinitionsTo(_, defns))
    }

    def prepend[B](implicit ev: A <:< Expr[B]): Expr[B] = {
      val expr = ev(usage)
      PrependValsTo.initializeDefns(defns, expr)(Expr.typeOf(expr))
    }
  }
  protected val PrependValsTo: PrependValsToModule
  protected trait PrependValsToModule { this: PrependValsTo.type =>

    def initializeDefns[To: Type](vals: Vector[(ExprPromiseName, ExistentialExpr, DefnType)], expr: Expr[To]): Expr[To]

    def setVal[To: Type](name: ExprPromiseName): Expr[To] => Expr[Unit]

    sealed trait DefnType extends scala.Product with Serializable
    object DefnType {
      case object Def extends DefnType
      case object Lazy extends DefnType
      case object Val extends DefnType
      case object Var extends DefnType
    }
  }

  implicit protected val PrependValsToTraversableApplicative: fp.ApplicativeTraverse[PrependDefinitionsTo] =
    new fp.ApplicativeTraverse[PrependDefinitionsTo] {

      def map2[A, B, C](fa: PrependDefinitionsTo[A], fb: PrependDefinitionsTo[B])(
          f: (A, B) => C
      ): PrependDefinitionsTo[C] = fa.map2(fb)(f)

      def pure[A](a: A): PrependDefinitionsTo[A] = new PrependDefinitionsTo[A](a, Vector.empty)

      def traverse[G[_]: fp.Applicative, A, B](fa: PrependDefinitionsTo[A])(f: A => G[B]): G[PrependDefinitionsTo[B]] =
        fa.traverse(f)
    }

  final protected class PatternMatchCase[To](
      val someFrom: ExistentialType,
      val usage: Expr[To],
      val fromName: ExprPromiseName
  )
  protected val PatternMatchCase: PatternMatchCaseModule
  protected trait PatternMatchCaseModule { this: PatternMatchCase.type =>

    final def unapply[To](patternMatchCase: PatternMatchCase[To]): Some[(ExistentialType, Expr[To], ExprPromiseName)] =
      Some((patternMatchCase.someFrom, patternMatchCase.usage, patternMatchCase.fromName))

    def matchOn[From: Type, To: Type](src: Expr[From], cases: List[PatternMatchCase[To]]): Expr[To]
  }

  implicit final protected class ListPatternMatchCaseOps[To: Type](cases: List[PatternMatchCase[To]]) {

    def matchOn[From: Type](src: Expr[From]): Expr[To] = PatternMatchCase.matchOn(src, cases)
  }
}
