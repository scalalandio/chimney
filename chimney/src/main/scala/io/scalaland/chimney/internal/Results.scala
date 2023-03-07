package io.scalaland.chimney.internal

import scala.annotation.tailrec
import scala.collection.compat._

trait Results {

  type DerivationConfig

  type Error
  val Error: ErrorCompanion
  trait ErrorCompanion { this: Error.type =>

    def fromException(throwable: Throwable): Error
    def mergeErrors(error1: Error, error2: Error): Error
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
      config: DerivationConfig,
      logNesting: Int = 0,
      logs: Vector[LogEntry] = Vector.empty
  ) {

    def appendLog(msg: => String): Context = copy(logs = logs :+ new LogEntry(logNesting, msg))
    def increaseLogNesting: Context = copy(logNesting = logNesting + 1)
    def decreaseLogNesting: Context = copy(logNesting = logNesting - 1)

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
  sealed trait Result[+A] {

    import Result._

    // monadic operations with sequential semantics (the first fail breaks the circuit)

    final def transformWith[B](onSuccess: A => Result[B])(onFailure: Error => Result[B]): Result[B] =
      TransformWith(this, onSuccess, onFailure)

    final def flatMap[B](f: A => Result[B]): Result[B] = transformWith(f)(fail)
    final def map[B](f: A => B): Result[B] = flatMap(f andThen pure)

    final def flatTap[B](f: A => Result[B]): Result[A] = flatMap(a => f(a).as(a))
    final def tap[B](f: A => B): Result[A] = flatTap(a => pure(a))

    final def recoverWith[A1 >: A](f: Error => Result[A1]): Result[A1] = transformWith[A1](pure)(f(_))
    final def recover[A1 >: A](f: Error => A1): Result[A1] = recoverWith(f andThen pure)

    final def map2[B, C](result: => Result[B])(f: (A, B) => C): Result[C] = flatMap(a => result.map(f(a, _)))

    final def as[B](value: B): Result[B] = map(_ => value)
    final def void: Result[Unit] = as(())

    final def >>[B](result: => Result[B]): Result[B] = flatMap(_ => result)
    final def <<[B](result: => Result[B]): Result[A] = flatMap(a => result.as(a))

    // applicative operations with parallel semantics (both branches are evaluated and then their results aggregated)

    final def parMap2[B, C](result: => Result[B])(f: (A, B) => C): Result[C] = context.flatMap { originalContext =>
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
          mergeContexts >> fail(Error.mergeErrors(errorA, errorB))
        }
      }
    }
    final def parTuple[B](result: => Result[B]): Result[(A, B)] = parMap2(result)(_ -> _)

    // evaluated until first success, if none succeed errors aggregate

    final def orElse[A1 >: A](result: => Result[A1]): Result[A1] = context.flatMap { context1 =>
      recoverWith { error =>
        val updateContext = UpdateContext(context2 => context1.merge(context2))
        result.transformWith { success =>
          updateContext.as(success)
        } { error2 =>
          updateContext >> fail(Error.mergeErrors(error, error2))
        }
      }
    }
  }

  object Result {

    type Value[O] = Either[Error, O]

    private final case class Pure[A](value: Value[A]) extends Result[A]
    private final case class TransformWith[A, B](result: Result[A], next: A => Result[B], fail: Error => Result[B])
        extends Result[B]
    private final case class UpdateContext(update: Context => Context) extends Result[Context]

    def apply[A](thunk: => A): Result[A] = unit.map(_ => thunk)
    def defer[A](thunk: => Result[A]): Result[A] = unit.flatMap(_ => thunk)

    def pure[A](value: A): Result[A] = Pure(Right(value))
    def fail[A](error: Error): Result[A] = Pure(Left(error))

    def context: Result[Context] = UpdateContext(identity)
    def config: Result[DerivationConfig] = context.map(_.config)
    def log(msg: String): Result[Unit] = UpdateContext(_.appendLog(msg)).void
    def nestLogs[A](result: Result[A]): Result[A] =
      UpdateContext(_.increaseLogNesting) >> result << UpdateContext(_.decreaseLogNesting)

    val unit: Result[Unit] = Pure(Right(()))

    type FactoryOf[Coll[+_], O] = Factory[O, Coll[O]]

    // monadic operations with sequential semantics (the first fail breaks the circuit)

    def traverse[C[+A] <: IterableOnce[A], I, O: FactoryOf[C, *]](coll: C[I])(f: I => Result[O]): Result[C[O]] =
      coll
        .foldLeft(pure(implicitly[FactoryOf[C, O]].newBuilder)) { (br, i) =>
          br.map2(f(i))(_ += _)
        }
        .map(_.result())
    def sequence[C[+A] <: IterableOnce[A], B: FactoryOf[C, *]](coll: C[Result[B]]): Result[C[B]] =
      traverse(coll)(identity)

    // applicative operations with parallel semantics (both branches are evaluated and then their results aggregated)

    def parTraverse[C[+A] <: IterableOnce[A], I, O: FactoryOf[C, *]](coll: C[I])(f: I => Result[O]): Result[C[O]] =
      coll
        .foldLeft(pure(implicitly[FactoryOf[C, O]].newBuilder)) { (br, i) =>
          br.parMap2(f(i))(_ += _)
        }
        .map(_.result())
    def parSequence[C[+A] <: IterableOnce[A], B: FactoryOf[C, *]](coll: C[Result[B]]): Result[C[B]] =
      parTraverse(coll)(identity)

    // evaluated until first success, if none succeed errors aggregate

    def firstOf[A](head: Result[A], tail: Result[A]*): Result[A] =
      tail.foldLeft(head)(_.orElse(_))

    // here be dragons

    final def unsafeRun[A](context: Context, result: Result[A]): (Context, Value[A]) = Stack.eval(context, result)

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
    private sealed trait Stack[-I, O]
    private object Stack {

      private object Ignored
      private val ignore = Right(Ignored)

      private final case class Rewrite[I, M, O](result: Result[M], tail: Stack[M, O])(implicit ev: I =:= Ignored.type)
          extends Stack[I, O]
      private final case class Advance[I, M, O](next: I => Result[M], fail: Error => Result[M], tail: Stack[M, O])
          extends Stack[I, O]
      private final case class Return[I, O](cast: I <:< O) extends Stack[I, O]

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
                case error: Throwable => context -> Left(Error.fromException(error))
              }
          }
          loop(newContext, next, stack)
        case Advance(onSuccess, onFailure, stack) =>
          val result =
            try current.fold(onFailure, onSuccess)
            catch { case err: Throwable => fail(Error.fromException(err)) }
          loop(context, ignore, Rewrite(result, stack))
        case Return(cast) =>
          context -> current.map(cast)
      }

      def eval[A](context: Context, result: Result[A]): (Context, Value[A]) =
        loop(context, ignore, Rewrite(result, Return(implicitly[A <:< A])))
    }
  }
}
