package io.scalaland.chimney.internal

import scala.annotation.tailrec

trait Results {

  type DerivationConfig

  type Error
  val Error: ErrorCompanion
  trait ErrorCompanion { this: Error.type =>

    def fromException(throwable: Throwable): Error
    def mergeErrors(error1: Error, error2: Error): Error
  }

  final class LogEntry(val nesting: Int, thunk: => String) {
    lazy val message: String = thunk
  }

  final case class Context(
      config: DerivationConfig,
      logNesting: Int,
      logs: Vector[LogEntry]
  ) {

    def appendLog(msg: => String): Context = copy(logs = logs :+ new LogEntry(logNesting, msg))
    def increaseLogNesting: Context = copy(logNesting = logNesting + 1)
    def decreaseLogNesting: Context = copy(logNesting = logNesting - 1)

    def merge(ctx: Context): Context = Context(
      config = config, // TODO? configs should be the same - consider logging warning if differ
      logNesting = logNesting, // TODO? log nesting should be the same - consider logging warning if differ
      logs = (logs ++ ctx.logs).distinct
    )
  }

  sealed trait Result[+A] {

    import Result._

    final def transformWith[B](onSuccess: A => Result[B])(onFailure: Error => Result[B]): Result[B] =
      TransformWith(this, onSuccess, onFailure)

    final def flatMap[B](f: A => Result[B]): Result[B] = transformWith(f)(fail)
    final def map[B](f: A => B): Result[B] = flatMap(f andThen pure)

    final def flatTap[B](f: A => Result[B]): Result[A] = flatMap(a => f(a).as(a))
    final def tap[B](f: A => B): Result[A] = flatTap(a => pure(a))

    final def recoverWith[A1 >: A](f: Error => Result[A1]): Result[A1] = transformWith[A1](pure)(f(_))
    final def recover[A1 >: A](f: Error => A1): Result[A1] = recoverWith(f andThen pure)

    final def parMap2[B, C](result: Result[B])(f: (A, B) => C): Result[C] = context.flatMap { originalContext =>
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
    final def parTuple[B](result: Result[B]): Result[(A, B)] = parMap2(result)(_ -> _)

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

    final def as[B](value: B): Result[B] = map(_ => value)
    final def void: Result[Unit] = as(())

    final def >>[B](result: => Result[B]): Result[B] = flatMap(_ => result)
    final def <<[B](result: => Result[B]): Result[A] = flatMap(a => result.as(a))
  }

  object Result {

    type Value[O] = Error Either O

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

    val unit: Result[Unit] = Pure(Right(()))

    def nestLogs[A](result: Result[A]): Result[A] =
      UpdateContext(_.increaseLogNesting) >> result << UpdateContext(_.decreaseLogNesting)

    // here be dragons

    final def unsafeRun[A](config: DerivationConfig, result: Result[A]): (Context, Result.Value[A]) =
      Stack.eval(config, result)

    /** Trampoline utility.
      *
      * all chain of monadic operations can be though of as something like:
      *
      *  {{{
      *  A => F[B] >=> B => F[C] >=> C => F[D] >=> D => F[E] >=> ...
      *  }}}
      *
      * where operations would be grouped together like e.g.
      *
      *  {{{
      *  A => F[B]
      *  >=>
      *  (
      *    (
      *      B => F[C]
      *      >=>
      *      C => F[D]
      *    )
      *    >=>
      *    D => F[E]
      *  )
      *  ...
      *  }}}
      *
      * Stack is used to rewrite this chain of operations so that the head will change from
      *
      * {{{
      * (
      *   A => F[B]
      *   >=>
      *   B => F[C]
      * )
      * >=>
      * C > F[D]
      * ...
      * }}}
      *
      * to
      *
      *
      * {{{
      * A => F[B]
      * >=>
      * (
      *   B => F[C]
      *   >=>
      *   C => F[D])
      * )
      * ...
      * }}}
      *
      * which would make it possible to evaluate the function at the head. By rewriting until evaluation is possible,
      * and then evaluating in a `loop`, we are able to execute the whole Result with stack safety.
      */
    private sealed trait Stack[I, O] {

      final def +:(r: Result[I]): Stack[Unit, O] = Stack.AndThen[Unit, I, O]((_: Unit) => r, fail[I](_), this)
    }
    private object Stack {

      private final case class Returns[I, O](ev: I <:< O) extends Stack[I, O]
      private final case class AndThen[I, M, O](next: I => Result[M], fail: Error => Result[M], tail: Stack[M, O])
          extends Stack[I, O]

      private def returns[O]: Stack[O, O] = Returns(implicitly[O <:< O])

      private def catchUpdate(context: Context, update: Context => Context): (Context, Value[Context]) =
        try {
          val newContext = update(context)
          newContext -> Right[Error, Context](newContext)
        } catch {
          case err: Throwable => context -> Left[Error, Context](Error.fromException(err))
        }
      private def catchResult[A](thunk: => Result[A]): Result[A] = {
        try {
          thunk
        } catch {
          case err: Throwable => Pure(Left(Error.fromException(err)))
        }
      }
      private val ignored: Value[Unit] = Right(())

      @tailrec
      private def loop[I, O](
          context: Context,
          current: Value[I],
          stack: Stack[I, O]
      ): (Context, Value[O]) = stack match {
        case Returns(ev) =>
          context -> current.map(ev)
        case AndThen(onSuccess, onFailure, tail) =>
          current match {
            case Left(error) =>
              catchResult(onFailure(error)) match {
                case Pure(value) => loop(context, value, tail)
                case result      => loop(context, ignored, result +: tail)
              }
            case Right(value) =>
              catchResult(onSuccess(value)) match {
                case Pure(newValue) =>
                  loop(context, newValue, tail)
                case TransformWith(newResult, onSuccess2, onFailure2) =>
                  val rewritten = newResult +: AndThen(onSuccess2, onFailure2, tail)
                  loop(context, ignored, rewritten)
                case UpdateContext(update) =>
                  val (newContext, newValue) = catchUpdate(context, update)
                  loop(newContext, newValue, tail)
              }
          }
      }

      def eval[A](config: DerivationConfig, result: Result[A]): (Context, Value[A]) =
        loop(Context(config, 0, Vector.empty), Right(()), result +: returns)
    }
  }
}
