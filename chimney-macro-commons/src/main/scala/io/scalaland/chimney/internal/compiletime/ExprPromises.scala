package io.scalaland.chimney.internal.compiletime

import scala.collection.immutable.ListMap

private[compiletime] trait ExprPromises { this: Definitions =>

  /** In Scala 2 it's `c.universe.TermName`, in Scala 3 `Symbol` of a val */
  protected type ExprPromiseName

  /** Allow us to use `Expr[A]` before we would either: know how we would initiate it, or: what the final shape of a
    * whole expression would be.
    *
    * In situations like `'{ val a = sth; ${ useA('{ a }) } }` you know both how `a` would be created as well as the
    * shape of the final tree. In cases when you would e.g. use expression in some context-dependent derivation which
    * could return `Either[Expr[B], Expr[F[B]]`, ExprPromise allows you to calculate that result and THEN decide how to
    * turn it into final Expr value.
    *
    * @tparam From
    *   type of the promised expression
    * @tparam A
    *   type of the current result we created using Expr[From]
    */
  final protected class ExprPromise[From: Type, A](private val usage: A, private val fromName: ExprPromiseName) {

    def map[B](f: A => B): ExprPromise[From, B] = new ExprPromise(f(usage), fromName)

    def traverse[G[_]: fp.Functor, B](f: A => G[B]): G[ExprPromise[From, B]] = {
      import fp.Implicits.*
      f(usage).map(new ExprPromise(_, fromName))
    }

    private def fulfilAsDefinition(
        init: Expr[From],
        definitionType: PrependDefinitionsTo.DefnType
    ): PrependDefinitionsTo[A] =
      new PrependDefinitionsTo(usage, Vector((fromName, ExistentialExpr[From](init), definitionType)))

    def fulfilAsDef(init: Expr[From]): PrependDefinitionsTo[A] =
      fulfilAsDefinition(init, PrependDefinitionsTo.DefnType.Def)
    def fulfilAsLazy(init: Expr[From]): PrependDefinitionsTo[A] =
      fulfilAsDefinition(init, PrependDefinitionsTo.DefnType.Lazy)
    def fulfilAsVal(init: Expr[From]): PrependDefinitionsTo[A] =
      fulfilAsDefinition(init, PrependDefinitionsTo.DefnType.Val)
    def fulfilAsVar(init: Expr[From]): PrependDefinitionsTo[(A, Expr[From] => Expr[Unit])] =
      fulfilAsDefinition(init, PrependDefinitionsTo.DefnType.Var).map(_ -> PrependDefinitionsTo.setVal(fromName))

    def fulfilAsLambda[To: Type](implicit ev: A <:< Expr[To]): Expr[From => To] =
      ExprPromise.createLambda(fromName, ev(usage))
    def fulfilAsLambda2[From2: Type, B, To: Type](promise: ExprPromise[From2, B])(
        combine: (A, B) => Expr[To]
    ): Expr[(From, From2) => To] =
      ExprPromise.createLambda2(fromName, promise.fromName, combine(usage, promise.usage))

    def fulfillAsPatternMatchCase[To](implicit ev: A <:< Expr[To]): PatternMatchCase[To] =
      new PatternMatchCase(
        someFrom = Type[From].as_??,
        usage = ev(usage),
        fromName = fromName
      )

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

    /** Creates the expression promise.
      *
      * @param nameGenerationStrategy
      *   to avoid accidental name clashing, we are using fresh name generator which assures us that the name would be
      *   unique, we are only choosing the prefix
      * @param usageHint
      *   if we'll fulfil promise as val/lazy val/var it let us decide as which
      * @tparam From
      *   type of promised expression
      */
    final def promise[From: Type](
        nameGenerationStrategy: NameGenerationStrategy,
        usageHint: UsageHint = UsageHint.None
    ): ExprPromise[From, Expr[From]] = {
      val name = provideFreshName[From](nameGenerationStrategy, usageHint: UsageHint)
      new ExprPromise(createRefToName[From](name), name)
    }

    protected def provideFreshName[From: Type](
        nameGenerationStrategy: NameGenerationStrategy,
        usageHint: UsageHint
    ): ExprPromiseName
    protected def createRefToName[From: Type](name: ExprPromiseName): Expr[From]
    def createLambda[From: Type, To: Type, B](
        fromName: ExprPromiseName,
        to: Expr[To]
    ): Expr[From => To]
    def createLambda2[From: Type, From2: Type, To: Type, B](
        fromName: ExprPromiseName,
        from2Name: ExprPromiseName,
        to: Expr[To]
    ): Expr[(From, From2) => To]

    sealed trait NameGenerationStrategy extends Product with Serializable
    object NameGenerationStrategy {
      final case class FromPrefix(src: String) extends NameGenerationStrategy
      case object FromType extends NameGenerationStrategy
      final case class FromExpr[A](expr: Expr[A]) extends NameGenerationStrategy
    }

    /** An artifact of a leaky abstraction.
      *
      * In Scala 2 we can create new identifier as s `String`, pass it around and wrap it with `Ident`/`TermName` to use
      * the value. Whether it would become val, var, lazy val, def etc could be deferred with no consequences.
      *
      * On Scala `Ident` takes `Symbol`, this `Symbol` has to be created with the flags and a type. And flags decide
      * whether it would become : var (Flags.Mutable), lazy val (Flags.Lazy), binding on pattern matching, parameter...
      *
      * For normal val, def, binding, method parameter - we can get away with something like:
      *
      * {{{
      * (methodParameterWithNewIdentifier: Type) => {
      *   val nameGeneratedBefore = methodParameterWithNewIdentifier
      *   // code using nameGeneratedBefore
      * }
      * }}}
      *
      * but it cannot be used for `lazy val`s or `var`s.
      */
    sealed trait UsageHint extends Product with Serializable
    object UsageHint {
      case object None extends UsageHint
      case object Lazy extends UsageHint
      case object Var extends UsageHint
    }
  }

  implicit protected def ExprPromiseTraverse[From]: fp.Traverse[ExprPromise[From, *]] =
    new fp.Traverse[ExprPromise[From, *]] {

      def traverse[G[_]: fp.Applicative, A, B](fa: ExprPromise[From, A])(f: A => G[B]): G[ExprPromise[From, B]] =
        fa.traverse(f)

      def parTraverse[G[_]: fp.Parallel, A, B](fa: ExprPromise[From, A])(f: A => G[B]): G[ExprPromise[From, B]] =
        fa.traverse(f)
    }

  /** When we decide that promised expression would be used as val/lazy val/var/def, we receive this wrapper around the
    * results, which would ensure that: initialization of a definition would happen before its use, you can only use the
    * definition inside its scope.
    */
  final protected class PrependDefinitionsTo[A](
      private val usage: A,
      private val defns: Vector[(ExprPromiseName, ExistentialExpr, PrependDefinitionsTo.DefnType)]
  ) {

    def map[B](f: A => B): PrependDefinitionsTo[B] = new PrependDefinitionsTo(f(usage), defns)

    def map2[B, C](val2: PrependDefinitionsTo[B])(f: (A, B) => C): PrependDefinitionsTo[C] =
      new PrependDefinitionsTo(f(usage, val2.usage), defns ++ val2.defns)

    def traverse[G[_]: fp.Functor, B](f: A => G[B]): G[PrependDefinitionsTo[B]] = {
      import fp.Implicits.*
      f(usage).map(new PrependDefinitionsTo(_, defns))
    }

    def closeBlockAsExprOf[B: Type](implicit ev: A <:< Expr[B]): Expr[B] =
      PrependDefinitionsTo.initializeDefns[B](defns, ev(usage))

    def use[B: Type](f: A => Expr[B]): Expr[B] = map(f).closeBlockAsExprOf
  }
  protected val PrependDefinitionsTo: PrependDefinitionsToModule
  protected trait PrependDefinitionsToModule { this: PrependDefinitionsTo.type =>

    def prependDef[From: Type](
        init: Expr[From],
        nameGenerationStrategy: ExprPromise.NameGenerationStrategy
    ): PrependDefinitionsTo[Expr[From]] =
      ExprPromise.promise[From](nameGenerationStrategy, ExprPromise.UsageHint.None).fulfilAsDef(init)
    def prependVal[From: Type](
        init: Expr[From],
        nameGenerationStrategy: ExprPromise.NameGenerationStrategy
    ): PrependDefinitionsTo[Expr[From]] =
      ExprPromise.promise[From](nameGenerationStrategy, ExprPromise.UsageHint.None).fulfilAsVal(init)
    def prependLazyVal[From: Type](
        init: Expr[From],
        nameGenerationStrategy: ExprPromise.NameGenerationStrategy
    ): PrependDefinitionsTo[Expr[From]] =
      ExprPromise.promise[From](nameGenerationStrategy, ExprPromise.UsageHint.Lazy).fulfilAsLazy(init)
    def prependVar[From: Type](
        init: Expr[From],
        nameGenerationStrategy: ExprPromise.NameGenerationStrategy
    ): PrependDefinitionsTo[(Expr[From], Expr[From] => Expr[Unit])] =
      ExprPromise.promise[From](nameGenerationStrategy, ExprPromise.UsageHint.Var).fulfilAsVar(init)

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

  implicit protected val PrependDefinitionsToTraversableApplicative: fp.ApplicativeTraverse[PrependDefinitionsTo] =
    new fp.ApplicativeTraverse[PrependDefinitionsTo] {

      def map2[A, B, C](fa: PrependDefinitionsTo[A], fb: PrependDefinitionsTo[B])(
          f: (A, B) => C
      ): PrependDefinitionsTo[C] = fa.map2(fb)(f)

      def pure[A](a: A): PrependDefinitionsTo[A] = new PrependDefinitionsTo[A](a, Vector.empty)

      def traverse[G[_]: fp.Applicative, A, B](fa: PrependDefinitionsTo[A])(f: A => G[B]): G[PrependDefinitionsTo[B]] =
        fa.traverse(f)

      def parTraverse[G[_]: fp.Parallel, A, B](fa: PrependDefinitionsTo[A])(f: A => G[B]): G[PrependDefinitionsTo[B]] =
        fa.traverse(f)
    }

  /** When we decide that expression would be crated in patter-match binding, we would receive this wrapper around the
    * results, which would ensure that definition is only used inside the scope and allow combining several cases into a
    * single pattern matching.
    */
  final protected class PatternMatchCase[To](
      val someFrom: ??,
      val usage: Expr[To],
      val fromName: ExprPromiseName
  )
  protected val PatternMatchCase: PatternMatchCaseModule
  protected trait PatternMatchCaseModule { this: PatternMatchCase.type =>

    final def unapply[To](patternMatchCase: PatternMatchCase[To]): Some[(??, Expr[To], ExprPromiseName)] =
      Some((patternMatchCase.someFrom, patternMatchCase.usage, patternMatchCase.fromName))

    def matchOn[From: Type, To: Type](src: Expr[From], cases: List[PatternMatchCase[To]]): Expr[To]
  }

  implicit final protected class ListPatternMatchCaseOps[To: Type](cases: List[PatternMatchCase[To]]) {

    def matchOn[From: Type](src: Expr[From]): Expr[To] = PatternMatchCase.matchOn(src, cases)
  }

  /** Allows "caching" some `Expr[A] => F[Expr[B]]` derivation as a `def`.
    *
    * @tparam F
    *   type of the result that would be cached method's body (including failed derivation)
    */
  abstract protected class DefCache[F[_]: fp.DirectStyle] {
    // Soo... neither a =:= b nor Type.isSameAs(a, b) want to work with Type[?] :/
    implicit private class TypeAnyOps[A](private val tpe: Type[A]) {
      def asAny: Type[Any] = tpe.asInstanceOf[Type[Any]]
    }

    // Key of the internal map of derived bodies
    private case class Signature(name: String, input: List[Type[Any]], output: Type[Any]) {

      override def hashCode(): Int = name.hashCode

      override def equals(obj: Any): Boolean = obj match {
        case Signature(name2, input2, output2) =>
          name == name2 && input.length == input2.length && input
            .zip(input2)
            .forall(p => p._1 =:= p._2) && output =:= output2
        case _ => false
      }

      override def toString: String =
        s"def $name(${input.map(Type.prettyPrint(_)).mkString(", ")}): ${Type.prettyPrint(output)}"
    }
    private object Signature {
      def apply[In1: Type, Out: Type](name: String) =
        new Signature(name, List(Type[In1].asAny), Type[Out].asAny)
      def apply[In1: Type, In2: Type, Out: Type](name: String) =
        new Signature(name, List(Type[In1].asAny, Type[In2].asAny), Type[Out].asAny)
    }

    // Allows accessing def inside its body
    protected trait PendingDef {
      def cast[A]: A
    }

    // Allows accessing def once it's defined
    protected trait Def {
      def prependDef[A: Type](expr: Expr[A]): Expr[A]
      def cast[A]: A
    }

    // Platform-specific way of creating def out of its body
    protected trait Define[In, Out] {
      def apply(body: In => Out): Def
      def pending: PendingDef
    }

    private var pending = ListMap.empty[Signature, PendingDef]
    private var defined = ListMap.empty[Signature, Def]

    private def unsafeGet[A](signature: Signature): Option[A] =
      defined.get(signature).map(_.cast[A]).orElse(pending.get(signature).map(_.cast[A]))

    final class Builder[In, Out, A] private (
        private val signature: Signature,
        private val body: In => A,
        private val define: Define[In, Out]
    ) {
      def map[B](f: A => B): Builder[In, Out, B] =
        new Builder[In, Out, B](signature, body andThen f, define)
      def emap[B](f: A => F[B]): Builder[In, Out, B] =
        new Builder[In, Out, B](signature, body andThen f andThen fp.DirectStyle[F].awaitUnsafe, define)
      def build(implicit ev: (In => A) =:= (In => Out)): Unit = {
        val evaluated = define(ev(body))
        pending = pending - signature
        defined = defined + (signature -> evaluated)
      }
    }
    private object Builder {
      def apply[In, Out](signature: Signature, define: Define[In, Out]): Builder[In, Out, In] = {
        val evaluated = define.pending
        pending = pending + (signature -> evaluated)
        new Builder[In, Out, In](signature, identity[In](_), define)
      }
    }

    final def build1[In1: Type, Out: Type](name: String): Builder[Expr[In1], Expr[Out], Expr[In1]] =
      Builder(Signature[In1, Out](name), define1[In1, Out](name))
    final def build2[In1: Type, In2: Type, Out: Type](
        name: String
    ): Builder[(Expr[In1], Expr[In2]), Expr[Out], (Expr[In1], Expr[In2])] =
      Builder(Signature[In1, In2, Out](name), define2[In1, In2, Out](name))

    final def of1[In1: Type, Out: Type](name: String): Option[Expr[In1] => F[Expr[Out]]] =
      unsafeGet[Expr[In1] => Expr[Out]](Signature[In1, Out](name)).map(fn =>
        in1 => fp.DirectStyle[F].asyncUnsafe(fn(in1))
      )
    final def of2[In1: Type, In2: Type, Out: Type](name: String): Option[(Expr[In1], Expr[In2]) => F[Expr[Out]]] =
      unsafeGet[(Expr[In1], Expr[In2]) => Expr[Out]](Signature[In1, In2, Out](name)).map(fn =>
        (in1, in2) => fp.DirectStyle[F].asyncUnsafe(fn(in1, in2))
      )

    final def prependDefs[A: Type](expr: Expr[A]): F[Expr[A]] = fp.DirectStyle[F].asyncUnsafe {
      defined.values.foldRight(expr)((def0, e) => def0.prependDef(e))
    }

    // Platform-specific implementations
    protected def define1[In1: Type, Out: Type](name: String): Define[Expr[In1], Expr[Out]]
    protected def define2[In1: Type, In2: Type, Out: Type](name: String): Define[(Expr[In1], Expr[In2]), Expr[Out]]

    override def toString: String =
      s"DefCache(pending = Seq(${pending.keys.mkString(", ")}), defined = Seq(${defined.keys.mkString(", ")}))"
  }

  protected val DefCache: DefCacheModule
  protected trait DefCacheModule { this: DefCache.type =>

    def apply[F[_]: fp.DirectStyle]: DefCache[F]
  }
}
