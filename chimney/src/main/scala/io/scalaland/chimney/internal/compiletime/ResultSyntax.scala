package io.scalaland.chimney.internal.compiletime

import hearth.fp.effect.{Log, MIO}

/** Log utilities missing from (or spelled differently on) [[hearth.fp.effect.MIO]], plus `orElseOpt`.
  *
  * Mixed into [[ChimneyDefinitions]] so that everything in the cake (rules included) sees the extensions without
  * imports - package-object implicits are not visible in nested packages such as `derivation.transformer.rules`.
  */
private[compiletime] trait ResultSyntax {

  /** NOTE: an extension cannot be named `log` (MIO already has a member `object log`), hence `logInfo` - and MIO's own
    * `.log.info(msg)` logs only on success, while `.logInfo(msg)` appends regardless of success/failure.
    */
  implicit final class MioOps[A](private val result: MIO[A]) {

    /** Appends the message to the log regardless of success/failure. */
    def logInfo(msg: => String): MIO[A] = result.attemptFlatTap(_ => Log.info(msg))

    /** Logs of `f` land in a nested, named scope (MIO has no instance counterpart of [[Log.namedScope]]). */
    def namedScope[B](scopeName: String)(f: A => MIO[B]): MIO[B] =
      result.flatMap(a => Log.namedScope(scopeName)(f(a)))

    /** Like `orElse` (errors of ALL failed alternatives aggregate), but the alternative is optional and only evaluated
      * on failure.
      */
    def orElseOpt[A1 >: A](resultOpt: => Option[MIO[A1]]): MIO[A1] =
      result.handleErrorWith { err1 =>
        resultOpt match {
          case Some(alternative) => alternative.handleErrorWith(err2 => MIO.fail(err1 ++ err2))
          case None              => MIO.fail(err1)
        }
      }
  }
}
