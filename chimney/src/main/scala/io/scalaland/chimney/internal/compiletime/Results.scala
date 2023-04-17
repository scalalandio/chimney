package io.scalaland.chimney.internal.compiletime

import io.scalaland.chimney.internal.{TransformerDerivationError as DerivationError, *}

import scala.annotation.nowarn
import scala.collection.compat.*
import scala.util.control.NonFatal

@nowarn("msg=The outer reference in this type test cannot be checked at run time.")
private[compiletime] trait Results { this: Definitions =>

  // TODO: consider expanding
  trait Trace {
    def sourceTypeName: String
    def targetTypeName: String
  }
  protected object Trace {

    // used when we need to log potential errors before TransformerContext is created
    def forBeforeContext[Source: Type, Target: Type]: Trace = new Trace {
      // TODO: platform-specific way of printing type
      override val sourceTypeName: String = Type[Source].toString
      override val targetTypeName: String = Type[Target].toString
    }

    // used when we know that there will be no fail
    val empty: Trace = new Trace {
      override val sourceTypeName: String = ""
      override val targetTypeName: String = ""
    }
  }

  sealed protected trait Log extends Product with Serializable
  protected object Log {

    /** Single log entry with lazy evaluation (some messages can be expensive to create) */
    final case class Entry(msg: () => String) extends Log {
      lazy val message: String = msg()
    }
    object Entry {
      def defer(msg: => String): Entry = new Entry(msg = () => msg)
    }

    /** Collection of logs (single or nested) */
    final case class Journal(logs: Vector[Log]) {

      def append(msg: => String): Journal = copy(logs = logs :+ Entry.defer(msg))
    }

    /** Contains a collection of logs computed in a named, nested scope */
    final case class Scope(scopeName: String, journal: Journal) extends Log
  }

  final protected case class DerivationErrors(head: DerivationError, tail: Vector[DerivationError]) {

    def ++(errors: DerivationErrors): DerivationErrors =
      DerivationErrors(head, tail ++ Vector(errors.head) ++ errors.tail)

    def prettyPrint: String = DerivationError.printErrors(head +: tail)
  }
  protected object DerivationErrors {

    def apply(error: DerivationError, errors: DerivationError*): DerivationErrors =
      apply(error, errors.toVector)

    def fromException(throwable: Throwable)(implicit t: Trace): DerivationErrors =
      apply(MacroException(throwable, t.sourceTypeName, t.targetTypeName), Vector.empty)

    def notYetImplemented(what: String)(implicit t: Trace): DerivationErrors =
      apply(NotYetImplemented(what, t.sourceTypeName, t.targetTypeName), Vector.empty)
  }

  /** Representations of a ongoing computation.
   *
   * Features:
   * - handles errors
   * - catches exceptions
   * - provides sequential and parallel combinators
   *
   * Intended to simplify how we express our logic during the derivation without long types and boilerplate.
   */
  sealed protected trait DerivationResult[+A] {

    import DerivationResult.*

    private def updateState(update: State => State): DerivationResult[A] = this match {
      case Success(value, state)            => Success(value, update(state))
      case Failure(derivationErrors, state) => Failure(derivationErrors, update(state))
    }

    // monadic operations with sequential semantics (the first fail breaks the circuit)

    final def transformWith[B](
        onSuccess: A => DerivationResult[B]
    )(
        onFailure: DerivationErrors => DerivationResult[B]
    )(implicit t: Trace): DerivationResult[B] = {
      var state: State = null.asInstanceOf[State]

      val result =
        try {
          this match {
            case Success(value, s) =>
              state = s
              onSuccess(value)
            case Failure(derivationErrors, s) =>
              state = s
              onFailure(derivationErrors)
          }
        } catch {
          case NonFatal(err) => DerivationResult.fromException(err)
        }

      result.updateState(_.appendedTo(state))
    }

    final def flatMap[B](f: A => DerivationResult[B])(implicit t: Trace): DerivationResult[B] = transformWith(f)(fail)
    final def map[B](f: A => B)(implicit t: Trace): DerivationResult[B] = flatMap(f andThen pure)

    final def flatTap[B](f: A => DerivationResult[B])(implicit t: Trace): DerivationResult[A] = flatMap(a => f(a).as(a))
    final def tap[B](f: A => B)(implicit t: Trace): DerivationResult[A] = flatTap(a => pure(a))

    final def recoverWith[A1 >: A](f: DerivationErrors => DerivationResult[A1])(implicit
        t: Trace
    ): DerivationResult[A1] =
      transformWith[A1](pure)(f(_))
    final def recover[A1 >: A](f: DerivationErrors => A1)(implicit t: Trace): DerivationResult[A1] =
      recoverWith(f andThen pure)

    final def map2[B, C](result: => DerivationResult[B])(f: (A, B) => C)(implicit t: Trace): DerivationResult[C] =
      flatMap(a => result.map(f(a, _)))

    final def as[B](value: B): DerivationResult[B] = map(_ => value)(Trace.empty)
    final def void: DerivationResult[Unit] = as(())

    final def >>[B](result: => DerivationResult[B])(implicit t: Trace): DerivationResult[B] = flatMap(_ => result)
    final def <<[B](result: => DerivationResult[B])(implicit t: Trace): DerivationResult[A] = flatMap(a => result.as(a))

    // applicative operations with parallel semantics (both branches are evaluated and then their results aggregated)

    final def parMap2[B, C](
        result: DerivationResult[B]
    )(f: (A, B) => C)(implicit t: Trace): DerivationResult[C] = transformWith { a =>
      result.map(b => f(a, b))
    } { errors =>
      result.transformWith(_ => fail(errors))(errors2 => fail(errors ++ errors2))
    }

    final def parTuple[B](result: => DerivationResult[B])(implicit t: Trace): DerivationResult[(A, B)] =
      parMap2(result)(_ -> _)

    // evaluated until first success, if none succeed errors aggregate

    final def orElse[A1 >: A](result: => DerivationResult[A1])(implicit t: Trace): DerivationResult[A1] =
      transformWith[A1](pure) { err1 =>
        result.transformWith(pure) { err2 =>
          fail(err1 ++ err2)
        }
      }

    // logging

    final def log(msg: => String): DerivationResult[A] = updateState(_.log(msg))

    final def namedScope[B](
        scopeName: String
    )(f: A => DerivationResult[B])(implicit t: Trace): DerivationResult[B] = flatMap { a =>
      f(a).updateState(_.nestScope(scopeName))
    }

    // conversion

    final def toEither: (State, Either[DerivationErrors, A]) = this match {
      case Success(value, state)            => state -> Right(value)
      case Failure(derivationErrors, state) => state -> Left(derivationErrors)
    }

    final def unsafeGet: (State, A) = this match {
      case Success(value, state)        => state -> value
      case Failure(derivationErrors, _) => reportError(derivationErrors.prettyPrint) // TODO: print state?
    }
  }
  protected object DerivationResult {

    final case class State(
        journal: Log.Journal = Log.Journal(logs = Vector.empty)
    ) {

      private[DerivationResult] def log(msg: => String): State = copy(journal = journal.append(msg))

      private[DerivationResult] def nestScope(scopeName: String): State =
        copy(journal = Log.Journal(Vector(Log.Scope(scopeName = scopeName, journal = journal))))

      private[DerivationResult] def appendedTo(previousState: State): State = State(
        journal = Log.Journal(logs = previousState.journal.logs ++ this.journal.logs)
      )
    }

    final private case class Success[A](value: A, state: State) extends DerivationResult[A]
    final private case class Failure(derivationErrors: DerivationErrors, state: State) extends DerivationResult[Nothing]

    def apply[A](thunk: => A)(implicit t: Trace): DerivationResult[A] = unit.map(_ => thunk)

    def pure[A](value: A): DerivationResult[A] = Success(value, State())
    def fail[A](error: DerivationErrors): DerivationResult[A] = Failure(error, State())

    val unit: DerivationResult[Unit] = pure(())

    def fromException[T](error: Throwable)(implicit t: Trace): DerivationResult[T] =
      fail(DerivationErrors.fromException(error))
    def notYetImplemented[T](what: String)(implicit t: Trace): DerivationResult[T] =
      fail(DerivationErrors.notYetImplemented(what))

    type FactoryOf[Coll[+_], O] = Factory[O, Coll[O]]

    // monadic operations with sequential semantics (the first fail breaks the circuit)

    def traverse[C[+A] <: IterableOnce[A], I, O: FactoryOf[C, *]](
        coll: C[I]
    )(f: I => DerivationResult[O])(implicit t: Trace): DerivationResult[C[O]] =
      coll.iterator
        .foldLeft(pure(implicitly[FactoryOf[C, O]].newBuilder)) { (br, i) =>
          br.map2(f(i))(_ += _)
        }
        .map(_.result())

    def sequence[C[+A] <: IterableOnce[A], B: FactoryOf[C, *]](coll: C[DerivationResult[B]])(implicit
        t: Trace
    ): DerivationResult[C[B]] =
      traverse(coll)(identity)

    // applicative operations with parallel semantics (both branches are evaluated and then their results aggregated)

    def parTraverse[C[+A] <: IterableOnce[A], I, O: FactoryOf[C, *]](
        coll: C[I]
    )(f: I => DerivationResult[O])(implicit t: Trace): DerivationResult[C[O]] =
      coll.iterator
        .foldLeft(pure(implicitly[FactoryOf[C, O]].newBuilder)) { (br, i) =>
          br.parMap2(f(i))(_ += _)
        }
        .map(_.result())

    def parSequence[C[+A] <: IterableOnce[A], B: FactoryOf[C, *]](
        coll: C[DerivationResult[B]]
    )(implicit t: Trace): DerivationResult[C[B]] =
      parTraverse(coll)(identity)

    // evaluated until first success, if none succeed errors aggregate

    def firstOf[A](head: DerivationResult[A], tail: DerivationResult[A]*)(implicit t: Trace): DerivationResult[A] =
      tail.foldLeft(head)(_.orElse(_))

    // logging

    def log(msg: => String): DerivationResult[Unit] = unit.log(msg)

    def namedScope[A](name: String)(ra: => DerivationResult[A])(implicit t: Trace): DerivationResult[A] =
      unit.namedScope(name)(_ => ra)
  }

  protected def reportError(errors: String): Nothing
}
