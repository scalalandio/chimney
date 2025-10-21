package io.scalaland.chimney.internal.compiletime

import scala.collection.compat.*
import scala.util.control.NonFatal

/** Representations of an ongoing computation.
  *
  * Features:
  *   - handles errors
  *   - catches exceptions
  *   - provides sequential and parallel combinators
  *
  * Intended to simplify how we express our logic during the derivation without long types and boilerplate.
  */
sealed private[compiletime] trait DerivationResult[+A] {

  import DerivationResult.*

  def state: State

  private def updateState(update: State => State): DerivationResult[A] = this match {
    case Success(value, state)            => Success(value, update(state))
    case Failure(derivationErrors, state) => Failure(derivationErrors, update(state))
  }

  // monadic operations with sequential semantics (the first fail breaks the circuit)

  final def transformWith[B](
      onSuccess: A => DerivationResult[B]
  )(
      onFailure: DerivationErrors => DerivationResult[B]
  ): DerivationResult[B] = {
    var state: State = null.asInstanceOf[State]

    val result =
      try
        this match {
          case Success(value, s) =>
            state = s
            onSuccess(value)
          case Failure(derivationErrors, s) =>
            state = s
            onFailure(derivationErrors)
        }
      catch {
        case fatal @ FatalError(_, _) => throw fatal // unwind stack, we already saved the cause and last state
        case NonFatal(err)            => DerivationResult.fromException(err) // recoverable, turn into DerivationResult
        case fatal: Throwable => throw FatalError(fatal, this.state) // save last state and cause of a fatal error
      }

    result.updateState(_.appendedTo(state))
  }

  final def flatMap[B](f: A => DerivationResult[B]): DerivationResult[B] = transformWith(f)(fail)

  final def map[B](f: A => B): DerivationResult[B] = flatMap(f andThen pure)

  final def flatTap[B](f: A => DerivationResult[B]): DerivationResult[A] = flatMap(a => f(a).as(a))

  final def tap[B](f: A => B): DerivationResult[A] = flatTap { a =>
    val _ = f(a); pure(a)
  }

  final def recoverWith[A1 >: A](f: DerivationErrors => DerivationResult[A1]): DerivationResult[A1] =
    transformWith[A1](pure)(f(_))

  final def recover[A1 >: A](f: DerivationErrors => A1): DerivationResult[A1] =
    recoverWith(f andThen pure)

  final def map2[B, C](result: => DerivationResult[B])(f: (A, B) => C): DerivationResult[C] =
    flatMap(a => result.map(f(a, _)))

  final def as[B](value: B): DerivationResult[B] = map(_ => value)

  final def void: DerivationResult[Unit] = as(())

  final def >>[B](result: => DerivationResult[B]): DerivationResult[B] = flatMap(_ => result)

  final def <<[B](result: => DerivationResult[B]): DerivationResult[A] = flatMap(a => result.as(a))

  // applicative operations with parallel semantics (both branches are evaluated and then their results aggregated)

  final def parMap2[B, C](result: DerivationResult[B])(f: (A, B) => C): DerivationResult[C] = transformWith { a =>
    result.map(b => f(a, b))
  } { errors =>
    result.transformWith(_ => fail(errors))(errors2 => fail(errors ++ errors2))
  }

  final def parTuple[B](result: => DerivationResult[B]): DerivationResult[(A, B)] =
    parMap2(result)(_ -> _)

  // evaluated until first success, if none succeed errors aggregate

  final def orElse[A1 >: A](result: => DerivationResult[A1]): DerivationResult[A1] =
    transformWith[A1](pure) { err1 =>
      result.transformWith(pure) { err2 =>
        fail(err1 ++ err2)
      }
    }

  final def orElseOpt[A1 >: A](resultOpt: => Option[DerivationResult[A1]]): DerivationResult[A1] =
    transformWith[A1](pure) { err1 =>
      resultOpt match {
        case Some(result) =>
          result.transformWith(pure) { err2 =>
            fail(err1 ++ err2)
          }
        case None => fail(err1)
      }
    }

  // logging

  final def log(msg: => String): DerivationResult[A] = updateState(_.log(msg))

  final def logSuccess(msg: A => String): DerivationResult[A] = this match {
    case Success(value, _) => log(msg(value))
    case _: Failure        => this
  }

  final def logFailure(msg: DerivationErrors => String): DerivationResult[A] = this match {
    case _: Success[?]                => this
    case Failure(derivationErrors, _) => log(msg(derivationErrors))
  }

  final def namedScope[B](scopeName: String)(f: A => DerivationResult[B]): DerivationResult[B] = flatMap { a =>
    f(a).updateState(_.nestScope(scopeName))
  }

  // conversion

  final def toEither: Either[DerivationErrors, A] = this match {
    case Success(value, _)            => Right(value)
    case Failure(derivationErrors, _) => Left(derivationErrors)
  }
}
private[compiletime] object DerivationResult {

  final case class State(
      journal: Log.Journal = Log.Journal(logs = Vector.empty),
      macroLogging: Option[State.MacroLogging] = None
  ) {

    private[DerivationResult] def log(msg: => String): State = copy(journal = journal.append(msg))

    private[DerivationResult] def nestScope(scopeName: String): State =
      copy(journal = Log.Journal(Vector(Log.Scope(scopeName = scopeName, journal = journal))))

    private[DerivationResult] def appendedTo(previousState: State): State = State(
      journal = Log.Journal(logs = previousState.journal.logs ++ this.journal.logs),
      macroLogging = previousState.macroLogging.orElse(macroLogging)
    )
  }
  object State {
    final case class MacroLogging(derivationStartedAt: java.time.Instant)
  }

  final private case class Success[A](value: A, state: State) extends DerivationResult[A]
  final private case class Failure(derivationErrors: DerivationErrors, state: State) extends DerivationResult[Nothing]

  def apply[A](thunk: => A): DerivationResult[A] = unit.map(_ => thunk)
  def pure[A](value: A): DerivationResult[A] = Success(value, State())
  def fail[A](error: DerivationErrors): DerivationResult[A] = Failure(error, State())

  val unit: DerivationResult[Unit] = pure(())

  def fromException[A](error: Throwable): DerivationResult[A] =
    fail(DerivationErrors(DerivationError.MacroException(error)))
  def assertionError[A](msg: String): DerivationResult[A] =
    fromException(new AssertionError(msg))
  def notYetImplemented[A](what: String): DerivationResult[A] =
    fail(DerivationErrors(DerivationError.NotYetImplemented(what)))
  def transformerError[A](transformerDerivationError: TransformerDerivationError): DerivationResult[A] =
    fail(DerivationErrors(DerivationError.TransformerError(transformerDerivationError)))
  def patcherError[A](patcherDerivationError: PatcherDerivationError): DerivationResult[A] =
    fail(DerivationErrors(DerivationError.PatcherError(patcherDerivationError)))

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

  // logging

  def log(msg: => String): DerivationResult[Unit] = unit.log(msg)

  def namedScope[A](name: String)(ra: => DerivationResult[A]): DerivationResult[A] =
    unit.namedScope(name)(_ => ra)

  def enableLogPrinting(derivationStartedAt: java.time.Instant): DerivationResult[Unit] =
    unit.updateState(_.copy(macroLogging = Some(State.MacroLogging(derivationStartedAt))))

  def catchFatalErrors[A](result: => DerivationResult[A]): DerivationResult[A] = try
    result
  catch {
    case FatalError(fatal, state) => Failure(DerivationErrors(DerivationError.MacroException(fatal)), state)
  }

  // direct style

  final private case class PassErrors(derivationErrors: DerivationErrors, owner: Await[?]) extends Throwable
  sealed private[compiletime] trait Await[A] {
    def apply(dr: DerivationResult[A]): A
  }

  def direct[A, B](thunk: Await[A] => B): DerivationResult[B] = {
    var stateCache: State = null.asInstanceOf[State]
    val await = new Await[A] {
      def apply(dr: DerivationResult[A]): A = dr match {
        case Success(value, state) =>
          stateCache = state
          value
        case Failure(derivationErrors, state) =>
          stateCache = state
          throw PassErrors(derivationErrors, this)
      }
    }
    try {
      val result = thunk(await)
      Success(result, if (stateCache != null) stateCache else State()) // if await wasn't called stateCache is null
    } catch {
      case PassErrors(derivationErrors, `await`) => Failure(derivationErrors, stateCache)
      case NonFatal(error)                       => DerivationResult.fromException(error)
    }
  }

  implicit val DerivationResultTraversableApplicative: fp.ParallelTraverse[DerivationResult] =
    new fp.ParallelTraverse[DerivationResult] {

      def map2[A, B, C](fa: DerivationResult[A], fb: DerivationResult[B])(f: (A, B) => C): DerivationResult[C] =
        fa.map2(fb)(f)

      def parMap2[A, B, C](fa: DerivationResult[A], fb: DerivationResult[B])(f: (A, B) => C): DerivationResult[C] =
        fa.parMap2(fb)(f)

      def pure[A](a: A): DerivationResult[A] = DerivationResult.pure(a)

      def traverse[G[_]: fp.Applicative, A, B](fa: DerivationResult[A])(f: A => G[B]): G[DerivationResult[B]] = {
        import fp.Implicits.*
        fa match {
          case Success(value, state) => f(value).map(Success(_, state))
          case failure: Failure      => (failure: DerivationResult[B]).pure[G]
        }
      }

      def parTraverse[G[_]: fp.Parallel, A, B](fa: DerivationResult[A])(f: A => G[B]): G[DerivationResult[B]] = {
        import fp.Implicits.*
        fa match {
          case Success(value, state) => f(value).map(Success(_, state))
          case failure: Failure      => (failure: DerivationResult[B]).pure[G]
        }
      }
    }

  private case class FatalError(error: Throwable, state: State) extends Exception(error)
}
