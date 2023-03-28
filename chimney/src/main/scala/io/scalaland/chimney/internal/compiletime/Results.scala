package io.scalaland.chimney.internal.compiletime

import scala.annotation.nowarn
import scala.collection.compat.*
import scala.util.control.NonFatal

@nowarn("msg=The outer reference in this type test cannot be checked at run time.")
private[compiletime] trait Results { this: Definitions =>

  sealed protected trait DerivationError extends Product with Serializable
  protected object DerivationError {

    // TODO: expand as needed
    final case class MacroException(throwable: Throwable) extends DerivationError
    final case class NotYetImplemented(what: String) extends DerivationError
  }

  final protected case class DerivationErrors(head: DerivationError, tail: Vector[DerivationError]) {

    def ++(errors: DerivationErrors): DerivationErrors =
      DerivationErrors(head, tail ++ Vector(errors.head) ++ errors.tail)

    def prettyPrint: String = toString // TODO
  }
  protected object DerivationErrors {

    def apply(error: DerivationError, errors: DerivationError*): DerivationErrors =
      apply(error, errors.toVector)

    def fromException(throwable: Throwable): DerivationErrors =
      apply(DerivationError.MacroException(throwable), Vector.empty)

    def notYetImplemented(what: String): DerivationErrors =
      apply(DerivationError.NotYetImplemented(what), Vector.empty)
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

    // monadic operations with sequential semantics (the first fail breaks the circuit)

    final def transformWith[B](
        onSuccess: A => DerivationResult[B]
    )(
        onFailure: DerivationErrors => DerivationResult[B]
    ): DerivationResult[B] = try {
      this match {
        case Success(value)            => onSuccess(value)
        case Failure(derivationErrors) => onFailure(derivationErrors)
      }
    } catch {
      case NonFatal(err) => DerivationResult.fromException(err)
    }

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

    final def parMap2[B, C](result: DerivationResult[B])(f: (A, B) => C): DerivationResult[C] = transformWith { a =>
      result.map(b => f(a, b))
    } { errors =>
      result.transformWith(_ => fail(errors))(errors2 => fail(errors ++ errors2))
    }

    final def parTuple[B](result: => DerivationResult[B]): DerivationResult[(A, B)] = parMap2(result)(_ -> _)

    // evaluated until first success, if none succeed errors aggregate

    final def orElse[A1 >: A](result: => DerivationResult[A1]): DerivationResult[A1] = transformWith[A1](pure) { err1 =>
      result.transformWith(pure) { err2 =>
        fail(err1 ++ err2)
      }
    }

    // conversion

    final def toEither: Either[DerivationErrors, A] = this match {
      case Success(value)            => Right(value)
      case Failure(derivationErrors) => Left(derivationErrors)
    }

    final def unsafeGet: A = this match {
      case Success(value)            => value
      case Failure(derivationErrors) => reportError(derivationErrors.prettyPrint)
    }
  }
  protected object DerivationResult {

    final private case class Success[A](value: A) extends DerivationResult[A]
    final private case class Failure(derivationErrors: DerivationErrors) extends DerivationResult[Nothing]

    def apply[A](thunk: => A): DerivationResult[A] = unit.map(_ => thunk)

    def pure[A](value: A): DerivationResult[A] = Success(value)
    def fail[A](error: DerivationErrors): DerivationResult[A] = Failure(error)

    val unit: DerivationResult[Unit] = pure(())

    def fromException[T](error: Throwable): DerivationResult[T] = fail(DerivationErrors.fromException(error))
    def notYetImplemented[T](what: String): DerivationResult[T] = fail(DerivationErrors.notYetImplemented(what))

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
  }

  protected def reportError(errors: String): Nothing
}
