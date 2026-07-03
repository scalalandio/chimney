package io.scalaland.chimney.internal.compiletime

import hearth.fp.DirectStyle
import hearth.fp.effect.{Log, MIO, MLocal}

import scala.collection.Factory
import scala.util.control.NonFatal

/** Companion-like module for `DerivationResult` (a type alias for `MIO[A]`, see the package object).
  *
  * Semantics pins:
  *   - `orElse`/`firstOf`: errors of ALL failed alternatives aggregate (`fail(e1 ++ e2)`),
  *   - `parMap2`/`parTraverse`/`parSequence`: both branches run even if the first failed and the errors aggregate,
  *   - exception catching: MIO catches `NonFatal` in every combinator; caught raw `Throwable`s are classified as
  *     [[DerivationError.MacroException]] at RENDERING time ([[DerivationError.fromThrowable]]),
  *   - fatal errors (e.g. `StackOverflowError`) propagate out of `unsafe.runSync` - the Gateway wraps `runSync` in a
  *     `try`/`catch` to keep the "increase -Xss" message.
  */
private[compiletime] object DerivationResult {

  def apply[A](thunk: => A): DerivationResult[A] = MIO(thunk)

  def pure[A](value: A): DerivationResult[A] = MIO.pure(value)
  def fail[A](error: DerivationErrors): DerivationResult[A] = MIO.fail(error)

  val unit: DerivationResult[Unit] = MIO.void

  def fromException[A](error: Throwable): DerivationResult[A] =
    fail(DerivationErrors(DerivationError.fromThrowable(error)))
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
    MIO.firstOf(head, tail*)

  // logging

  def log(msg: => String): DerivationResult[Unit] = Log.info(msg)

  def namedScope[A](name: String)(ra: => DerivationResult[A]): DerivationResult[A] =
    Log.namedScope(name)(ra)

  /** "The macro-logging flag was enabled, dump the log journal at the end of the derivation (in Gateway)". */
  final case class MacroLogging(derivationStartedAt: java.time.Instant)

  /** Macro-logging flag as an [[MLocal]] (set by [[enableLogPrinting]], read - inside the program - by the Gateway). */
  val macroLogging: MLocal[Option[MacroLogging]] =
    MLocal(Option.empty[MacroLogging])(identity)((a, b) => a.orElse(b))

  def enableLogPrinting(derivationStartedAt: java.time.Instant): DerivationResult[Unit] =
    macroLogging.set(Some(MacroLogging(derivationStartedAt)))

  // direct style

  /** The value extractor passed to [[direct]]'s body. MIO's `RunSafe` is polymorphic - the type parameter only exists
    * so that call sites with explicit type applications compile.
    */
  type Await[A] = DirectStyle.RunSafe[MIO]

  def direct[A, B](thunk: Await[A] => B): DerivationResult[B] =
    MIO
      .scoped { runSafe =>
        // Turns NonFatal exceptions of the block itself into failures (MIO's `scoped` lets them fly); `RunSafe`'s
        // own error-passing uses a ControlThrowable, which NonFatal does not intercept, so awaiting failed results
        // still works.
        try Right(thunk(runSafe))
        catch { case NonFatal(error) => Left(error) }
      }
      .flatMap {
        case Right(value) => pure(value)
        case Left(error)  => fromException(error)
      }
}
