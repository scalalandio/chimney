package io.scalaland.chimney.internal.compiletime2

/** Gathers all possible derivation errors in a single type.
  *
  * Hearth-based port of `io.scalaland.chimney.internal.compiletime.DerivationError`. Differences vs the old version:
  *   - reparented on a stackless `Exception` (instead of plain `Product with Serializable`) so instances can travel
  *     inside MIO's `MErrors` (`NonEmptyVector[Throwable]`) - the `message` only serves debugging (e.g. MIO's own
  *     "Caught exception ..." logs), user-facing rendering still goes through [[DerivationError.printErrors]], which is
  *     preserved byte-identical,
  *   - [[DerivationError.fromThrowable]] added - MIO can catch arbitrary `Throwable`s, which must be (re)classified as
  *     [[DerivationError.MacroException]] before rendering.
  */
sealed abstract class DerivationError(message: String) extends Exception(message, null, false, false)
object DerivationError {

  final case class MacroException(exception: Throwable)
      extends DerivationError(s"macro expansion thrown exception!: $exception")
  final case class NotYetImplemented(what: String)
      extends DerivationError(s"derivation failed because functionality $what is not yet implemented!")
  final case class TransformerError(transformerDerivationError: TransformerDerivationError)
      extends DerivationError(transformerDerivationError.toString)
  final case class PatcherError(patcherDerivationError: PatcherDerivationError)
      extends DerivationError(patcherDerivationError.toString)

  /** Classifies an arbitrary `Throwable` caught by MIO as a [[DerivationError]] (old code never needed this since
    * `DerivationResult` wrapped exceptions in `MacroException` at the catch site).
    */
  def fromThrowable(error: Throwable): DerivationError = error match {
    case derivationError: DerivationError => derivationError
    case _                                => MacroException(error)
  }

  def printErrors(derivationErrors: Seq[DerivationError]): String =
    derivationErrors
      .collectFirst {
        case MacroException(exception) =>
          val stackTrace =
            exception.getStackTrace.view
              .take(10)
              .map(ste => s"  \t${Console.RED}$ste${Console.RESET}")
              .mkString("\n") + s"\n  \t${Console.RED}...${Console.RESET}"
          exception match {
            case _: StackOverflowError =>
              s"  macro expansion thrown StackOverflow - usually it's a sign that JVM need larger Stack. Increase it with e.g. -Xss64m passed to JVM!: $exception:\n$stackTrace"
            case _ =>
              s"  macro expansion thrown exception!: $exception:\n$stackTrace"
          }
        case NotYetImplemented(what) =>
          s"  derivation failed because functionality $what is not yet implemented!"
      }
      .getOrElse {
        val transformerErrors = derivationErrors.collect { case TransformerError(transformerDerivationError) =>
          transformerDerivationError
        }
        val patcherErrors = derivationErrors.collect { case PatcherError(patcherDerivationError) =>
          patcherDerivationError
        }

        (transformerErrors, patcherErrors) match {
          case (Seq(), Seq()) =>
            ""
          case (tErrs, Seq()) =>
            TransformerDerivationError.printErrors(tErrs)
          case (Seq(), pErrs) =>
            PatcherDerivationError.printErrors(pErrs)
          case (tErrs, pErrs) =>
            TransformerDerivationError.printErrors(tErrs) ++ "\n" ++ PatcherDerivationError.printErrors(pErrs)
        }
      }
}
