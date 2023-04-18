package io.scalaland.chimney.internal.compiletime

import io.scalaland.chimney.internal.TransformerDerivationError

sealed trait DerivationError extends Product with Serializable
object DerivationError {

  final case class MacroException(exception: Throwable) extends DerivationError
  final case class NotYetImplemented(what: String) extends DerivationError
  final case class TransformerError(transformerDerivationError: TransformerDerivationError) extends DerivationError

  def printErrors(derivationErrors: Seq[DerivationError]): String =
    derivationErrors
      .collectFirst {
        case MacroException(exception) =>
          val stackTrace = exception.getStackTrace.view.map(ste => s"  \t$ste").mkString("\n")
          s"  macro expansion thrown exception!: ${exception.getMessage}:\n$stackTrace"
        case NotYetImplemented(what) =>
          s"  derivation failed because functionality $what is not yet implemented!"
      }
      .getOrElse {
        TransformerDerivationError.printErrors(derivationErrors.collect {
          case TransformerError(transformerDerivationError) => transformerDerivationError
        })
      }
}
