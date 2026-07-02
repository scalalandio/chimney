package io.scalaland.chimney.internal.compiletime

import hearth.fp.effect.{Log, MErrors, MIO}

/** Syntax bringing the old `DerivationResult`/`DerivationErrors` instance methods onto their MIO-based replacements.
  *
  * Mixed into [[ChimneyDefinitions]] so that everything in the cake (rules included) sees the extensions without
  * imports - like the old instance methods. (They cannot live in the package object: package-object implicits are not
  * visible in nested packages such as `derivation.transformer.rules` - verified experimentally on 2.13 and 3.)
  */
private[compiletime] trait ResultSyntax {

  /** Old `DerivationErrors#prettyPrint`/`#asVector`, now as syntax on [[hearth.fp.effect.MErrors]]. */
  implicit final class DerivationErrorsOps(private val errors: MErrors) {

    def prettyPrint: String = DerivationError.printErrors(errors.toVector.map(DerivationError.fromThrowable))

    def asVector: Vector[DerivationError] = errors.toVector.map(DerivationError.fromThrowable)
  }

  /** Old `DerivationResult` combinators missing from (or spelled differently on) [[hearth.fp.effect.MIO]], as syntax.
    *
    * Name mapping for the mechanical rules port (methods that could not keep their old names, or that MIO already
    * provides under a different name):
    *   - `.log(msg)` -> `.logInfo(msg)` (MIO already has a member `object log`, which makes an extension named `log`
    *     unusable; NOTE: MIO's own `.log.info(msg)` logs only on success, while `.logInfo(msg)` keeps the old "append
    *     regardless of success/failure" semantics),
    *   - `.tap(f)` -> `.tap(f)` (alias for MIO's `.mapTap`),
    *   - `.transformWith(onSuccess)(onFailure)` -> same name (alias for MIO's `.redeemWith`),
    *   - `.recoverWith(f)`/`.recover(f)` - use MIO's own methods: they take `PartialFunction`s, but total-function
    *     literals at the old call sites adapt automatically (with identical semantics),
    *   - `.toEither`/`.state` - GONE: MIO is lazy, results exist only after `unsafe.runSync` (Gateway concern).
    */
  implicit final class DerivationResultOps[A](private val result: DerivationResult[A]) {

    /** Old `DerivationResult#log`: appends the message to the log regardless of success/failure. */
    def logInfo(msg: => String): DerivationResult[A] = result.attemptFlatTap(_ => Log.info(msg))

    /** Old `DerivationResult#logSuccess`: appends the message only if the result is a success. */
    def logSuccess(msg: A => String): DerivationResult[A] = result.log.valueAsInfo(msg)

    /** Old `DerivationResult#logFailure`: appends the message only if the result is a failure. */
    def logFailure(msg: DerivationErrors => String): DerivationResult[A] = result.log.errorsAsInfo(msg)

    /** Old `DerivationResult#namedScope`: logs of `f` land in a nested, named scope. */
    def namedScope[B](scopeName: String)(f: A => DerivationResult[B]): DerivationResult[B] =
      result.flatMap(a => Log.namedScope(scopeName)(f(a)))

    /** Old `DerivationResult#transformWith` (MIO calls it `redeemWith`, with both handlers in one parameter list). */
    def transformWith[B](
        onSuccess: A => DerivationResult[B]
    )(
        onFailure: DerivationErrors => DerivationResult[B]
    ): DerivationResult[B] = result.redeemWith(onSuccess)(onFailure)

    /** Old `DerivationResult#tap` (MIO calls it `mapTap`). */
    def tap[B](f: A => B): DerivationResult[A] = result.mapTap(f)

    /** Old `DerivationResult#<<` (MIO spells the sequential variant `<*`). */
    def <<[B](fb: => DerivationResult[B]): DerivationResult[A] = result <* fb

    /** Old `DerivationResult#orElseOpt`: like `orElse` (errors of ALL failed alternatives aggregate), but the
      * alternative is optional and only evaluated on failure.
      */
    def orElseOpt[A1 >: A](resultOpt: => Option[DerivationResult[A1]]): DerivationResult[A1] =
      result.handleErrorWith { err1 =>
        resultOpt match {
          case Some(alternative) => alternative.handleErrorWith(err2 => MIO.fail(err1 ++ err2))
          case None              => MIO.fail(err1)
        }
      }
  }
}
