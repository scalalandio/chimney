package io.scalaland.chimney.internal.compiletime

import scala.annotation.nowarn

@nowarn("msg=The outer reference in this type test cannot be checked at run time.")
private[compiletime] trait ResultDefinitionsPlatform extends ResultDefinitions { this: DefinitionsPlatform =>

  final override protected def reportOrReturn[A](context: Context, value: DerivationResult.Value[A]): A = {
    value match {
      case Left(errors) =>
        val output = new StringBuilder()
        output.append("Derivation failed due to following errors:\n")
        (errors.head +: errors.tail).foreach {
          case DerivationError.MacroException(throwable) =>
            output.append(s"- Unexpected exception was thrown: ${throwable.getMessage}\n")
          case DerivationError.NotYetImplemented(functionality) =>
            output.append(s"- Expansion touched not yet implemented branch of code: $functionality")
        }
        output.append("\n")
        output.append("Consult the documentation")
        c.abort(c.enclosingPosition, output.result())
      case Right(value) =>
        value
    }
  }
}
