package io.scalaland.chimney.internal.compiletime

import scala.annotation.{nowarn, tailrec}
import scala.collection.compat.*

@nowarn("msg=The outer reference in this type test cannot be checked at run time.")
private[compiletime] trait ResultDefinitions { this: Definitions =>

  sealed protected trait DerivationError extends Product with Serializable
  protected object DerivationError {

    // TODO: expand as needed
    final case class MacroException(throwable: Throwable) extends DerivationError
    final case class NotYetImplemented(functionality: String) extends DerivationError
  }

  final protected case class DerivationErrors(head: DerivationError, tail: Vector[DerivationError]) {

    def ++(errors: DerivationErrors): DerivationErrors =
      DerivationErrors(head, tail ++ Vector(errors.head) ++ errors.tail)
  }
  protected object DerivationErrors {

    def apply(error: DerivationError, errors: DerivationError*): DerivationErrors =
      apply(error, errors.toVector)

    def fromException(throwable: Throwable): DerivationErrors =
      apply(DerivationError.MacroException(throwable), Vector.empty)
  }

  /** Lazily evaluated log entry */
  final class LogEntry(val nesting: Int, thunk: => String) {
    lazy val message: String = thunk
  }

  /** Contains everything we would like to access in our computations without resorting to globals.
   *
   * Possible usages:
   * - access to config
   * - appending logs and diagnostics
   * - storing cache for intermediate results
   */
  final case class Context(
      config: TransformerConfig = TransformerConfig(),
      logNesting: Int = 0,
      logs: Vector[LogEntry] = Vector.empty
  ) {

    def appendLog(msg: => String): Context = copy(logs = logs :+ new LogEntry(logNesting, msg))
    def increaseLogNesting: Context = copy(logNesting = logNesting + 1)
    def decreaseLogNesting: Context = copy(logNesting = logNesting - 1)

    // TODO: rethink if necessary
    def merge(ctx: Context): Context = copy(
      config = config, // TODO? configs should be the same - consider logging warning if differ
      logNesting = logNesting, // TODO? log nesting should be the same - consider logging warning if differ
      logs = (logs ++ ctx.logs).distinct
    )
  }

  /** Representations of a ongoing computation.
   *
   * Features:
   * - stack-safe
   * - handles errors
   * - catches exceptions
   * - provides sequential and parallel combinators
   * - threads context through computations
   *
   * Intended to simplify how we express our logic during the derivation without long types and boilerplate.
   */
  sealed protected trait DerivationResult[+A] {

    import DerivationResult.*

    // monadic operations with sequential semantics (the first fail breaks the circuit)

    final def transformWith[B](onSuccess: A => DerivationResult[B])(
        onFailure: DerivationErrors => DerivationResult[B]
    ): DerivationResult[B] =
      TransformWith(this, onSuccess, onFailure)

    final def flatMap[B](f: A => DerivationResult[B]): DerivationResult[B] = transformWith(f)(fail)
    final def map[B](f: A => B): DerivationResult[B] = flatMap(f andThen pure)

    final def flatTap[B](f: A => DerivationResult[B]): DerivationResult[A] = flatMap(a => f(a).as(a))
    final def tap[B](f: A => B): DerivationResult[A] = flatTap(a => pure(a))

    final def recoverWith[A1 >: A](f: DerivationErrors => DerivationResult[A1]): DerivationResult[A1] =
      transformWith[A1](pure)(f(_))
    final def recover[A1 >: A](f: DerivationErrors => A1): DerivationResult[A1] = recoverWith(f andThen pure)

    final def map2[B, C](result: => DerivationResult[B])(f: (A, B) => C): DerivationResult[C] =
      flatMap(a => result.map(f(a, _)))

    final def as[B](value: B): DerivationResult[B] = map(_ => value)
    final def void: DerivationResult[Unit] = as(())

    final def >>[B](result: => DerivationResult[B]): DerivationResult[B] = flatMap(_ => result)
    final def <<[B](result: => DerivationResult[B]): DerivationResult[A] = flatMap(a => result.as(a))

    // applicative operations with parallel semantics (both branches are evaluated and then their results aggregated)

    final def parMap2[B, C](result: => DerivationResult[B])(f: (A, B) => C): DerivationResult[C] = context.flatMap {
      originalContext =>
        // TODO: thread context without restarting

        var contextA: Context = originalContext
        val rewindContext = UpdateContext(ctx => { contextA = ctx; ctx }).flatMap(_ => result)
        val mergeContexts = UpdateContext(contextB => contextA.merge(contextB))

        transformWith[C] { valueA =>
          rewindContext.transformWith { valueB =>
            mergeContexts.as(f(valueA, valueB))
          } { errorB =>
            mergeContexts >> fail(errorB)
          }
        } { errorA =>
          rewindContext.transformWith[C] { _ =>
            mergeContexts >> fail(errorA)
          } { errorB =>
            mergeContexts >> fail(errorA ++ errorB)
          }
        }
    }
    final def parTuple[B](result: => DerivationResult[B]): DerivationResult[(A, B)] = parMap2(result)(_ -> _)

    // evaluated until first success, if none succeed errors aggregate

    final def orElse[A1 >: A](result: => DerivationResult[A1]): DerivationResult[A1] = context.flatMap { context1 =>
      recoverWith { error =>
        val updateContext = UpdateContext(context2 => context1.merge(context2))
        result.transformWith { success =>
          updateContext.as(success)
        } { error2 =>
          updateContext >> fail(error ++ error2)
        }
      }
    }
  }
  protected object DerivationResult {

    type Value[O] = Either[DerivationErrors, O]

    final private case class Pure[A](value: Value[A]) extends DerivationResult[A]
    final private case class TransformWith[A, B](
        result: DerivationResult[A],
        next: A => DerivationResult[B],
        fail: DerivationErrors => DerivationResult[B]
    ) extends DerivationResult[B]
    final private case class UpdateContext(update: Context => Context) extends DerivationResult[Context]

    def apply[A](thunk: => A): DerivationResult[A] = unit.map(_ => thunk)
    def defer[A](thunk: => DerivationResult[A]): DerivationResult[A] = unit.flatMap(_ => thunk)

    def pure[A](value: A): DerivationResult[A] = Pure(Right(value))
    def fail[A](error: DerivationErrors): DerivationResult[A] = Pure(Left(error))

    def context: DerivationResult[Context] = UpdateContext(identity)
    def config: DerivationResult[TransformerConfig] = context.map(_.config)
    def log(msg: String): DerivationResult[Unit] = UpdateContext(_.appendLog(msg)).void
    // TODO: rethink
    def nestLogs[A](result: DerivationResult[A]): DerivationResult[A] =
      UpdateContext(_.increaseLogNesting) >> result << UpdateContext(_.decreaseLogNesting)

    val unit: DerivationResult[Unit] = Pure(Right(()))

    def fromException[T](error: Throwable): DerivationResult[T] = fail(DerivationErrors.fromException(error))
    def notYetImplemented[T](functionality: String): DerivationResult[T] = fail(
      DerivationErrors(DerivationError.NotYetImplemented(functionality))
    )

    type FactoryOf[Coll[+_], O] = Factory[O, Coll[O]]

    // monadic operations with sequential semantics (the first fail breaks the circuit)

    def traverse[C[+A] <: IterableOnce[A], I, O: FactoryOf[C, *]](
        coll: C[I]
    )(f: I => DerivationResult[O]): DerivationResult[C[O]] =
      coll.iterator
        .foldLeft(pure(implicitly[FactoryOf[C, O]].newBuilder)) { (br, i) =>
          br.map2(f(i))(_ += _)
        }
        .map(_.result())
    def sequence[C[+A] <: IterableOnce[A], B: FactoryOf[C, *]](coll: C[DerivationResult[B]]): DerivationResult[C[B]] =
      traverse(coll)(identity)

    // applicative operations with parallel semantics (both branches are evaluated and then their results aggregated)

    def parTraverse[C[+A] <: IterableOnce[A], I, O: FactoryOf[C, *]](
        coll: C[I]
    )(f: I => DerivationResult[O]): DerivationResult[C[O]] =
      coll.iterator
        .foldLeft(pure(implicitly[FactoryOf[C, O]].newBuilder)) { (br, i) =>
          br.parMap2(f(i))(_ += _)
        }
        .map(_.result())
    def parSequence[C[+A] <: IterableOnce[A], B: FactoryOf[C, *]](
        coll: C[DerivationResult[B]]
    ): DerivationResult[C[B]] =
      parTraverse(coll)(identity)

    // evaluated until first success, if none succeed errors aggregate

    def firstOf[A](head: DerivationResult[A], tail: DerivationResult[A]*): DerivationResult[A] =
      tail.foldLeft(head)(_.orElse(_))

    // here be dragons

    final def unsafeRun[A](context: Context, result: DerivationResult[A]): (Context, Value[A]) =
      Stack.eval(context, result)

    final def unsafeRunExpr[A](context: Context, result: DerivationResult[Expr[A]]): Expr[A] = {
      val (computedContext, value) = unsafeRun(context, result)
      reportOrReturn(computedContext, value)
    }

    /** Trampoline utility.
     *
     * all chain of monadic operations can be though of as something like:
     *
     *  {{{
     *  A => F[B] andThen B => F[C] andThen C => F[D] andThen D => F[E] andThen ...
     *  }}}
     *
     * where operations would be grouped together like e.g.
     *
     *  {{{
     *  A => F[B]
     *  andThen
     *  (
     *    (
     *      B => F[C]
     *      andThen
     *      C => F[D]
     *    )
     *    andThen
     *    D => F[E]
     *  )
     *  ...
     *  }}}
     *
     * Monadic laws guarantee us that only global order of operations is important, not how we group it inside
     * parenthesis, so we can rearrange them for our convenience.
     *
     * `Stack` is used exactly for that, to rewrite this chain of operations so that the head will change from e.g.
     *
     * {{{
     * (
     *   A => F[B]
     *   andThen
     *   B => F[C]
     * )
     * andThen
     * C > F[D]
     * ...
     * }}}
     *
     * to
     *
     * {{{
     * A => F[B]
     * andThen
     * (
     *   B => F[C]
     *   andThen
     *   C => F[D])
     * )
     * ...
     * }}}
     *
     * which would make it possible to evaluate the function at the head. By rewriting until evaluation is possible,
     * and then evaluating in a `loop`, we are able to execute the whole `Result` with stack safety.
     */
    sealed private trait Stack[-I, O]
    private object Stack {

      private object Ignored
      private val ignore = Right(Ignored)

      final private case class Rewrite[I, M, O](result: DerivationResult[M], tail: Stack[M, O])(implicit
          ev: I =:= Ignored.type
      ) extends Stack[I, O]
      final private case class Advance[I, M, O](
          next: I => DerivationResult[M],
          fail: DerivationErrors => DerivationResult[M],
          tail: Stack[M, O]
      ) extends Stack[I, O]
      final private case class Return[I, O](cast: I <:< O) extends Stack[I, O]

      @tailrec
      private def loop[I, O](context: Context, current: Value[I], s: Stack[I, O]): (Context, Value[O]) = s match {
        case Rewrite(Pure(next), stack) =>
          loop(context, next, stack)
        case Rewrite(TransformWith(result, onSuccess, onFailure), stack) =>
          loop(context, ignore, Rewrite(result, Advance(onSuccess, onFailure, stack)))
        case Rewrite(UpdateContext(update), stack) =>
          val (newContext, next) = current match {
            case Left(error) => context -> Left(error)
            case Right(_) =>
              try {
                val newContext = update(context)
                newContext -> Right(newContext)
              } catch {
                case error: Throwable => context -> Left(DerivationErrors.fromException(error))
              }
          }
          loop(newContext, next, stack)
        case Advance(onSuccess, onFailure, stack) =>
          val result =
            try current.fold(onFailure, onSuccess)
            catch { case err: Throwable => fail(DerivationErrors.fromException(err)) }
          loop(context, ignore, Rewrite(result, stack))
        case Return(cast) =>
          context -> current.map(cast)
      }

      def eval[A](context: Context, result: DerivationResult[A]): (Context, Value[A]) =
        loop(context, ignore, Rewrite(result, Return(implicitly[A <:< A])))
    }
  }

  protected def reportOrReturn[A](context: Context, value: DerivationResult.Value[A]): A
}