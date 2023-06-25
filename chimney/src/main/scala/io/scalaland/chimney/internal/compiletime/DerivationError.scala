package io.scalaland.chimney.internal.compiletime

import io.scalaland.chimney.internal.TransformerDerivationError

sealed private[compiletime] trait DerivationError extends Product with Serializable
private[compiletime] object DerivationError {

  final case class MacroException(exception: Throwable) extends DerivationError
  final case class NotYetImplemented(what: String) extends DerivationError
  final case class TransformerError(transformerDerivationError: TransformerDerivationError) extends DerivationError

  def printErrors(derivationErrors: Seq[DerivationError]): String =
    derivationErrors
      .collectFirst {
        case MacroException(exception) =>
          val stackTrace =
            exception.getStackTrace.view.take(10).map(ste => s"  \t${Console.RED}$ste${Console.RESET}").mkString("\n")
          s"  macro expansion thrown exception!: $exception:\n$stackTrace\n  \t${Console.RED}...${Console.RESET}"
        case NotYetImplemented(what) =>
          s"  derivation failed because functionality $what is not yet implemented!"
      }
      .getOrElse {
        TransformerDerivationError.printErrors(derivationErrors.collect {
          case TransformerError(transformerDerivationError) => transformerDerivationError
        })
      }
}
