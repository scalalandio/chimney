package io.scalaland.chimney.internal

sealed trait DerivationError {
  def sourceTypeName: String
  def targetTypeName: String
}

case class MissingField(fieldName: String, fieldTypeName: String, sourceTypeName: String, targetTypeName: String)
    extends DerivationError

case class MissingTransformer(fieldName: String,
                              sourceFieldTypeName: String,
                              targetFieldTypeName: String,
                              sourceTypeName: String,
                              targetTypeName: String)
    extends DerivationError

case class NotSupportedDerivation(sourceTypeName: String, targetTypeName: String) extends DerivationError

object DerivationError {

  def printErrors(errors: Seq[DerivationError]): String = {

    errors
      .groupBy(_.targetTypeName)
      .map {
        case (targetTypeName, errs) =>
          val errStrings = errs.map {
            case MissingField(fieldName, fieldTypeName, sourceTypeName, _) =>
              s"  $fieldName: $fieldTypeName - no field named $fieldName in source type $sourceTypeName"
            case MissingTransformer(fieldName, sourceFieldTypeName, targetFieldTypeName, sourceTypeName, _) =>
              s"  $fieldName: $targetFieldTypeName - can't derive transformation from $fieldName: $sourceFieldTypeName in source type $sourceTypeName"
            case NotSupportedDerivation(sourceTypeName, _) =>
              s"  derivation from $sourceTypeName is not supported in Chimney!"
          }

          s"""$targetTypeName
           |${errStrings.mkString("\n")}
           |""".stripMargin
      }
      .mkString("\n")
  }
}
