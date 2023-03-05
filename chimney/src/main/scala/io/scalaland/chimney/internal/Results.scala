package io.scalaland.chimney.internal

import scala.annotation.tailrec

trait Results {

  type DerivationConfig = Unit // TODO

  type Error = Unit // TODO
  private def fromException(throwable: Throwable): Error = () // TODO
  private def mergeErrors(error1: Error, error2: Error): Error = () // TODO

  final class LogEntry(thunk: => String) {
    lazy val msg: String = thunk
  }

  final case class Context(
      cfg: DerivationConfig,
      logs: Vector[LogEntry]
  ) {

    def appendLog(msg: => String): Context = copy(logs = logs :+ new LogEntry(msg))

    def merge(ctx: Context): Context = Context(cfg, logs ++ ctx.logs)
  }

  sealed trait Result[+A] {

    import Result._

    final def transformWith[B](onSuccess: A => Result[B])(onFailure: Error => Result[B]): Result[B] =
      TransformWith(this, onSuccess, onFailure)

    final def flatMap[B](f: A => Result[B]): Result[B] = transformWith(f)(fail)
    final def map[B](f: A => B): Result[B] = flatMap(f andThen pure)

    final def recoverWith[A1 >: A](f: Error => Result[A1]): Result[A1] = transformWith[A1](pure)(f(_))
    final def recover[A1 >: A](f: Error => A1): Result[A1] = recoverWith(f andThen pure)

    final def parMap2[B, C](result: Result[B])(f: (A, B) => C): Result[C] =
      context.flatMap { context =>
        var context1 = context
        val rewindContext = UpdateContext { ctx =>
          context1 = ctx
          context
        }.flatMap(_ => result)
        val mergeContexts = UpdateContext(context2 => context1.merge(context2))

        transformWith[C] { valueA =>
          rewindContext.transformWith { valueB =>
            mergeContexts.as(f(valueA, valueB))
          } { errorB =>
            mergeContexts.flatMap(_ => fail(errorB))
          }
        } { errorA =>
          rewindContext.transformWith[C] { valueA =>
            mergeContexts.flatMap(_ => fail(errorA))
          } { errorB =>
            mergeContexts.flatMap(_ => fail(mergeErrors(errorA, errorB)))
          }
        }
      }
    final def parTuple[B](result: Result[B]): Result[(A, B)] = parMap2(result)(_ -> _)

    final def orElse[A1 >: A](result: => Result[A1]): Result[A1] =
      context.flatMap { context1 =>
        recoverWith { error =>
          val updateContext = UpdateContext(context2 => context1 merge context2)
          result.transformWith { success =>
            updateContext.as(success)
          } { error2 =>
            updateContext.flatMap(_ => fail(mergeErrors(error, error2)))
          }
        }
      }

    final def as[B](value: B): Result[B] = map(_ => value)
    final def void: Result[Unit] = as(())
  }

  object Result {

    type Value[O] = Error Either O

    private final case class Pure[A](value: Value[A]) extends Result[A]
    private final case class TransformWith[A, B](result: Result[A], next: A => Result[B], fail: Error => Result[B])
        extends Result[B]
    private final case class UpdateContext(update: Context => Context) extends Result[Context]

    def pure[A](value: A): Result[A] = Pure(Right(value))
    def fail[A](error: Error): Result[A] = Pure(Left(error))

    def context: Result[Context] = UpdateContext(identity)
    def config: Result[DerivationConfig] = context.map(_.cfg)
    def log(msg: String): Result[Unit] = UpdateContext(_.appendLog(msg)).void

    // here be dragons

    def eval[A](config: DerivationConfig, result: Result[A]): (Context, Value[A]) =
      Stack.loop(Context(config, Vector.empty), Right(()), result &: Stack.returns)

    /* Trampoline utility.
     *
     * all chain of monadic operations can be though of as something like:
     *
     *  A => F[B], B => F[C], C => F[D], D => F[E], ...
     *
     * where operations would be grouped together like e.g.
     *
     *  (A => F[B], ((B => F[C], C => F[D]), D => F[E])), ...
     *
     * Stack is used to rewrite this chain of operations so that the head will change from
     *
     * ((A => F[B], B => F[C]), C => F[D]), ...
     *
     * to
     *
     * A => F[B], (B => F[C], C => F[D], ...)
     *
     * which would make it possible to evaluate the function at the head. By rewriting until evaluation is possible, and
     * then evaluating in a loop, we are able to execute the whole Result with stack safety.
     */
    private sealed trait Stack[I, O] {

      final def &:(r: Result[I]): Stack[Unit, O] = Stack.AndThen[Unit, I, O]((_: Unit) => r, fail[I](_), this)
    }
    private object Stack {

      private final case class Returns[I, O](ev: I <:< O) extends Stack[I, O]
      private final case class AndThen[I, M, O](next: I => Result[M], fail: Error => Result[M], tail: Stack[M, O])
          extends Stack[I, O]

      def returns[O]: Stack[O, O] = Returns(implicitly[O <:< O])

      private def catchError[A](thunk: => Result[A]): Result[A] =
        try {
          thunk
        } catch {
          case err: Throwable => Pure(Left(fromException(err)))
        }

      @tailrec
      def loop[I, O](context: Context, current: Value[I], stack: Stack[I, O]): (Context, Value[O]) = stack match {
        case Returns(ev) =>
          context -> current.map(ev)
        case AndThen(onSuccess, onFailure, tail) =>
          current match {
            case Left(error) =>
              catchError(onFailure(error)) match {
                case Pure(value) => loop(context, value, tail)
                case result      => loop(context, Right(()), result &: tail)
              }
            case Right(value) =>
              catchError(onSuccess(value)) match {
                case Pure(newValue) =>
                  loop(context, newValue, tail)
                case TransformWith(newResult, onSuccess2, onFailure2) =>
                  loop(
                    context,
                    Right(()),
                    newResult &: AndThen(onSuccess2, onFailure2, tail)
                  )
                case UpdateContext(update) =>
                  val (newContext: Context, newValue: Value[Context]) =
                    try {
                      val newContext = update(context)
                      newContext -> Right[Error, Context](newContext)
                    } catch {
                      case err: Throwable => context -> Left[Error, Context](fromException(err))
                    }
                  loop(newContext, newValue, tail)
              }
          }
      }
    }
  }
}
