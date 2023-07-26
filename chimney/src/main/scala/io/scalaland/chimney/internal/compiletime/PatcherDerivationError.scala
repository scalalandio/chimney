package io.scalaland.chimney.internal.compiletime

/** Patcher-specific error related to derivation logic */
sealed trait PatcherDerivationError extends Product with Serializable

final case class NotSupportedPatcherDerivation(objTypeName: String, patchTypeName: String)
    extends PatcherDerivationError

final case class PatchFieldNotFoundInTargetObj(patchFieldName: String, objTypeName: String)
    extends PatcherDerivationError

object PatcherDerivationError {

  def printError(patcherDerivationError: PatcherDerivationError): String = patcherDerivationError match {
    case NotSupportedPatcherDerivation(objTypeName, patchTypeName) =>
      s"Patcher derivation not supported for $objTypeName with patch type $patchTypeName"
    case PatchFieldNotFoundInTargetObj(patchFieldName, objTypeName) =>
      s"Field named '$patchFieldName' not found in target patching type $objTypeName!"
  }

  def printErrors(errors: Seq[PatcherDerivationError]): String =
    errors.map(printError).mkString("\n")
}
