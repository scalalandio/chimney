package io.scalaland.chimney.internal.compiletime

/** Gathers all possible derivation errors in a single type */
sealed trait DerivationError extends Product with Serializable
object DerivationError {

  final case class MacroException(exception: Throwable) extends DerivationError
  final case class NotYetImplemented(what: String) extends DerivationError
  final case class TransformerError(transformerDerivationError: TransformerDerivationError) extends DerivationError
  final case class PatcherError(patcherDerivationError: PatcherDerivationError) extends DerivationError

  def printErrors(derivationErrors: Seq[DerivationError]): String =
    derivationErrors
      .collectFirst {
        case MacroException(exception) =>
          val stackTrace =
            // exception.getStackTrace.view.take(10).map(ste => s"  \t${Console.RED}$ste${Console.RESET}").mkString("\n")
            exception.getStackTrace.view.map(ste => s"  \t${Console.RED}$ste${Console.RESET}").mkString("\n")
          s"  macro expansion thrown exception!: $exception:\n$stackTrace\n  \t${Console.RED}...${Console.RESET}"
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
