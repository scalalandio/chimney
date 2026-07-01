package io.scalaland.chimney.internal.compiletime2

import hearth.fp.DirectStyle
import hearth.fp.effect.{Log, MIO, MLocal}

import scala.collection.Factory
import scala.util.control.NonFatal

/** Companion-like module for the old `DerivationResult`, now backed by Hearth's [[hearth.fp.effect.MIO]].
  *
  * `DerivationResult[A]` itself is a type alias for `MIO[A]` (see the package object); this object maps the old
  * constructors/combinators onto MIO. Rules are expected to use MIO's own API directly - this bridge exists so that
  * shared infra and the mechanically-ported rules keep compiling with minimal edits.
  *
  * Semantics notes (verified against both implementations):
  *   - errors: old `DerivationErrors` (NEL of `DerivationError`) becomes `MErrors` (`NonEmptyVector[Throwable]`); the
  *     error ADTs are now stackless exceptions ([[DerivationError]]),
  *   - `orElse`/`firstOf`: MIO's `orElse` has EXACTLY the old semantics - errors of ALL failed alternatives aggregate
  *     (`fail(e1 ++ e2)`); `MIO.firstOf` is the same fold as the old `firstOf`,
  *   - `parMap2`/`parTraverse`/`parSequence`: both run the second branch even if the first failed and aggregate the
  *     errors; MIO additionally forks/joins `MLocal` state between the "parallel" branches,
  *   - exception catching: MIO catches `NonFatal` in every combinator (old code did it in `transformWith`); caught raw
  *     `Throwable`s are classified as [[DerivationError.MacroException]] at RENDERING time
  *     ([[DerivationError.fromThrowable]]) instead of at catch time,
  *   - fatal errors (e.g. `StackOverflowError`): old code smuggled them through `FatalError`+`catchFatalErrors`; with
  *     MIO they simply propagate out of `unsafe.runSync` - TODO(hearth-migration): the Gateway port must wrap `runSync`
  *     in a `try`/`catch` to keep the "increase -Xss" message (`DerivationError.printErrors` still renders it once
  *     given `MacroException(e: StackOverflowError)`),
  *   - logging: the old journal becomes Hearth's [[hearth.fp.effect.Log]]; old `State.macroLogging` becomes
  *     [[DerivationResult.macroLogging]] (an [[hearth.fp.effect.MLocal]]) - TODO(hearth-migration): Gateway must READ
  *     it inside the MIO program (e.g. `result.attempt.tuple(DerivationResult.macroLogging.get)`) because the final
  *     `MState`'s local-value accessor is `private[effect]` in Hearth; the log dump itself renders from
  *     `state.logs.render.fromInfo(...)` after `runSync`,
  *   - `direct`: mapped onto MIO's `DirectStyle` (`MIO.scoped`); the old monomorphic `Await[A]` is now an alias for the
  *     polymorphic `DirectStyle.RunSafe[MIO]` (the extra type parameter of `direct[A, B]` is kept so existing call
  *     sites with explicit type applications compile unchanged),
  *   - the old `fp.ParallelTraverse[DerivationResult]` instance is NOT ported: Hearth ships `Parallel[MIO]`
  *     (`MIO.ParallelForMio`), which is what collection `traverse`/`parTraverse` syntax needs.
  */
private[compiletime2] object DerivationResult {

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

  /** Old `DerivationResult.State.MacroLogging` - "the macro-logging flag was enabled, dump the log journal at the end
    * of the derivation (in Gateway)".
    */
  final case class MacroLogging(derivationStartedAt: java.time.Instant)

  /** Old `State#macroLogging`, now an [[MLocal]] (set by [[enableLogPrinting]], read by the Gateway). */
  val macroLogging: MLocal[Option[MacroLogging]] =
    MLocal(Option.empty[MacroLogging])(identity)((a, b) => a.orElse(b))

  def enableLogPrinting(derivationStartedAt: java.time.Instant): DerivationResult[Unit] =
    macroLogging.set(Some(MacroLogging(derivationStartedAt)))

  // direct style

  /** Old `Await[A]` - the value extractor passed to [[direct]]'s body. MIO's `RunSafe` is polymorphic, so the type
    * parameter is only kept for source compatibility with old call sites.
    */
  type Await[A] = DirectStyle.RunSafe[MIO]

  def direct[A, B](thunk: Await[A] => B): DerivationResult[B] =
    MIO
      .scoped { runSafe =>
        // Preserves the old `direct` behavior of turning NonFatal exceptions of the block itself into failures
        // (MIO's `scoped` lets them fly); `RunSafe`'s own error-passing uses a ControlThrowable, which NonFatal
        // does not intercept, so awaiting failed results still works.
        try Right(thunk(runSafe))
        catch { case NonFatal(error) => Left(error) }
      }
      .flatMap {
        case Right(value) => pure(value)
        case Left(error)  => fromException(error)
      }
}
