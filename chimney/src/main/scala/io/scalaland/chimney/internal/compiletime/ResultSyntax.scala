package io.scalaland.chimney.internal.compiletime

import hearth.fp.effect.{Log, MErrors, MIO}

/** `DerivationResult`/`DerivationErrors` syntax on top of MIO/MErrors.
  *
  * Mixed into [[ChimneyDefinitions]] so that everything in the cake (rules included) sees the extensions without
  * imports. (They cannot live in the package object: package-object implicits are not visible in nested packages such
  * as `derivation.transformer.rules` - verified experimentally on 2.13 and 3.)
  */
private[compiletime] trait ResultSyntax {

  /** `prettyPrint`/`asVector` syntax on [[hearth.fp.effect.MErrors]]. */
  implicit final class DerivationErrorsOps(private val errors: MErrors) {

    def prettyPrint: String = DerivationError.printErrors(errors.toVector.map(DerivationError.fromThrowable))

    def asVector: Vector[DerivationError] = errors.toVector.map(DerivationError.fromThrowable)
  }

  /** Combinators missing from (or spelled differently on) [[hearth.fp.effect.MIO]], as syntax.
    *
    * NOTE: an extension cannot be named `log` (MIO already has a member `object log`), hence `logInfo` - and MIO's own
    * `.log.info(msg)` logs only on success, while `.logInfo(msg)` appends regardless of success/failure.
    */
  implicit final class DerivationResultOps[A](private val result: DerivationResult[A]) {

    /** Appends the message to the log regardless of success/failure. */
    def logInfo(msg: => String): DerivationResult[A] = result.attemptFlatTap(_ => Log.info(msg))

    /** Appends the message only if the result is a success. */
    def logSuccess(msg: A => String): DerivationResult[A] = result.log.valueAsInfo(msg)

    /** Appends the message only if the result is a failure. */
    def logFailure(msg: DerivationErrors => String): DerivationResult[A] = result.log.errorsAsInfo(msg)

    /** Logs of `f` land in a nested, named scope. */
    def namedScope[B](scopeName: String)(f: A => DerivationResult[B]): DerivationResult[B] =
      result.flatMap(a => Log.namedScope(scopeName)(f(a)))

    /** Alias for MIO's `redeemWith`, with the handlers in separate parameter lists. */
    def transformWith[B](
        onSuccess: A => DerivationResult[B]
    )(
        onFailure: DerivationErrors => DerivationResult[B]
    ): DerivationResult[B] = result.redeemWith(onSuccess)(onFailure)

    /** Alias for MIO's `mapTap`. */
    def tap[B](f: A => B): DerivationResult[A] = result.mapTap(f)

    /** Alias for MIO's `<*`. */
    def <<[B](fb: => DerivationResult[B]): DerivationResult[A] = result <* fb

    /** Like `orElse` (errors of ALL failed alternatives aggregate), but the alternative is optional and only evaluated
      * on failure.
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
