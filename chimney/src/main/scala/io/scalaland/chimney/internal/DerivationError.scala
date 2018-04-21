package io.scalaland.chimney.internal

sealed trait DerivationError extends Product with Serializable {
  def sourceTypeName: String
  def targetTypeName: String
}

final case class MissingField(fieldName: String, fieldTypeName: String, sourceTypeName: String, targetTypeName: String)
    extends DerivationError

final case class MissingTransformer(fieldName: String,
                                    sourceFieldTypeName: String,
                                    targetFieldTypeName: String,
                                    sourceTypeName: String,
                                    targetTypeName: String)
    extends DerivationError

final case class CantFindValueClassMember(sourceTypeName: String, targetTypeName: String) extends DerivationError

final case class CantFindCoproductInstanceTransformer(instance: String, sourceTypeName: String, targetTypeName: String)
    extends DerivationError

final case class NotSupportedDerivation(sourceTypeName: String, targetTypeName: String) extends DerivationError

object DerivationError {

  def printErrors(errors: Seq[DerivationError]): String = {

    errors
      .groupBy(_.targetTypeName)
      .map {
        case (targetTypeName, errs) =>
          val errStrings = errs.distinct.map {
            case MissingField(fieldName, fieldTypeName, sourceTypeName, _) =>
              s"  $fieldName: $fieldTypeName - no field named $fieldName in source type $sourceTypeName"
            case MissingTransformer(fieldName, sourceFieldTypeName, targetFieldTypeName, sourceTypeName, _) =>
              s"  $fieldName: $targetFieldTypeName - can't derive transformation from $fieldName: $sourceFieldTypeName in source type $sourceTypeName"
            case CantFindValueClassMember(sourceTypeName, _) =>
              s"  can't find member of value class $sourceTypeName"
            case CantFindCoproductInstanceTransformer(instance, _, _) =>
              s"  can't transform coproduct instance $instance to $targetTypeName"
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
